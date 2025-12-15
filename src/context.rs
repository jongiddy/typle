mod expr;
mod generic_param;
mod pat;
mod shared;
mod r#type;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::ops::{Bound, Range};
use std::rc::Rc;

use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::{format_ident, ToTokens};
use syn::parse::Parser as _;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::{
    token, AttrStyle, Attribute, BinOp, Block, Expr, Fields, FnArg, GenericArgument, GenericParam,
    Generics, ImplItem, Item, Label, LifetimeParam, Lit, Macro, MacroDelimiter, Member, Meta, Pat,
    Path, PathArguments, QSelf, ReturnType, Signature, Stmt, Type, TypeParam, TypeParamBound,
    Visibility, WherePredicate,
};
use zip_clone::ZipClone as _;

use crate::constant::{evaluate_bool, evaluate_range, evaluate_usize};
use crate::context::shared::{abort, Replacements};
use crate::syn_ext::GeneralFunction;
use crate::TypleMacro;

#[derive(Clone)]
pub enum Typle {
    // `Typle<C>`: the name C of the type for all components (concrete or generic)
    Specific(Box<Type>),
    // `Typle`: the invented name for each component
    Generic(Rc<Vec<String>>),
}

impl Typle {
    pub fn get(&self, i: usize, span: Span) -> Type {
        match self {
            Typle::Specific(r#type) => (**r#type).clone(),
            Typle::Generic(v) => Type::Path(syn::TypePath {
                qself: None,
                path: ident_to_path(Ident::new(&v[i], span)),
            }),
        }
    }
}

#[derive(Default)]
pub struct BlockState {
    unlabelled_break: bool,
    unlabelled_continue: Option<Span>,
    labelled_control_flow: HashSet<Ident>,
    suspicious_ident: Option<Ident>,
}

impl BlockState {
    // Propagate the labels from an inner loop into the state for this loop.
    // We exclude any unlabelled_control_flow as it is contained by the inner
    // loop. We also exclude any label attached to the inner loop.
    fn propagate(&mut self, inner: Self, label: Option<&Label>) {
        for ident in inner.labelled_control_flow {
            if let Some(label) = label {
                if label.name.ident == ident {
                    continue;
                }
            }
            self.labelled_control_flow.insert(ident);
        }
    }

    // Return whether this loop has any labelled control flow that refers to
    // this loop's label.
    fn has_labelled_control_flow<'a>(&self, label: Option<&'a Label>) -> Option<&'a Label> {
        if let Some(label) = label {
            if self.labelled_control_flow.contains(&label.name.ident) {
                return Some(label);
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct TypleContext {
    typle_macro: Rc<TypleMacro>,
    typle_len: Option<usize>,
    // typle indexes that must be replaced with a number
    constants: HashMap<Ident, usize>,
    // Idents that must be renamed
    renames: HashMap<Ident, Cow<'static, str>>,
    retypes: HashMap<Ident, Type>,
    typles: HashMap<Ident, Typle>,
    const_if: bool,
}

impl From<Rc<TypleMacro>> for TypleContext {
    fn from(typle_macro: Rc<TypleMacro>) -> Self {
        TypleContext {
            typle_macro,
            typle_len: None,
            constants: HashMap::new(),
            renames: HashMap::new(),
            retypes: HashMap::new(),
            typles: HashMap::new(),
            const_if: false,
        }
    }
}

impl TypleContext {
    pub fn get_type(&self, typle: &Typle, i: usize, span: Span) -> syn::Result<Type> {
        match self.typle_len {
            Some(typle_len) => {
                if i < typle_len {
                    Ok(typle.get(i, span))
                } else if i < self.typle_macro.max_len {
                    Ok(self.typle_macro.never_type.clone())
                } else {
                    Err(syn::Error::new(span, "typle index out of range"))
                }
            }
            None => {
                if i < self.typle_macro.max_len {
                    Ok(typle.get(i, span))
                } else {
                    Err(syn::Error::new(span, "typle index out of range"))
                }
            }
        }
    }

    pub fn extract_typle_constraints(&self, generics: &mut Generics) -> syn::Result<Option<Self>> {
        fn get_type_ident(ty: &Type) -> Option<&Ident> {
            if let Type::Path(type_path) = ty {
                if type_path.qself.is_none() {
                    return type_path.path.get_ident();
                }
            }
            None
        }

        let mut context = None;

        for param in &mut generics.params {
            if let GenericParam::Type(type_param) = param {
                let type_ident = &type_param.ident;
                if let Some(typle) = self.typle_bounds(&mut type_param.bounds, type_ident)? {
                    let context = context.get_or_insert_with(|| self.clone());
                    context.typles.insert(type_ident.clone(), typle);
                }
            }
        }

        if let Some(where_clause) = generics.where_clause.as_mut() {
            let predicates = std::mem::take(&mut where_clause.predicates);
            for mut predicate in predicates {
                if let WherePredicate::Type(predicate_type) = &mut predicate {
                    if let Some(type_ident) = get_type_ident(&predicate_type.bounded_ty) {
                        if let Some(typle) =
                            self.typle_bounds(&mut predicate_type.bounds, type_ident)?
                        {
                            let context = context.get_or_insert_with(|| self.clone());
                            context.typles.insert(type_ident.clone(), typle);
                            if predicate_type.bounds.is_empty() {
                                continue;
                            }
                        }
                    }
                }
                where_clause.predicates.push(predicate);
            }
        }
        Ok(context)
    }

    fn typle_bounds(
        &self,
        bounds: &mut Punctuated<TypeParamBound, token::Plus>,
        type_ident: &Ident,
    ) -> syn::Result<Option<Typle>> {
        let mut result = None;
        for bound in std::mem::take(bounds) {
            if let TypeParamBound::Trait(trait_bound) = &bound {
                let path = &trait_bound.path;
                if path.leading_colon.is_none() && path.segments.len() == 1 {
                    if let Some(segment) = path.segments.first() {
                        if segment.ident == self.typle_macro.trait_ident {
                            match &segment.arguments {
                                PathArguments::None => {
                                    if result.is_some() {
                                        abort!(
                                            segment,
                                            format!(
                                                "constraint {:?} specified twice",
                                                segment.ident
                                            )
                                        );
                                    }
                                    result = Some(Typle::Generic(Rc::new(
                                        (0..self.typle_len.unwrap_or(self.typle_macro.max_len))
                                            .map(|i| format!("{}{}", &type_ident, i))
                                            .collect(),
                                    )));
                                    continue;
                                }
                                PathArguments::AngleBracketed(arguments) => {
                                    if arguments.args.len() != 1 {
                                        abort!(arguments, "expected single argument");
                                    }
                                    let Some(GenericArgument::Type(ty)) = arguments.args.first()
                                    else {
                                        abort!(arguments, "expected type");
                                    };
                                    if result.is_some() {
                                        abort!(
                                            segment,
                                            format!(
                                                "constraint {:?} specified twice",
                                                segment.ident
                                            )
                                        );
                                    }
                                    result = Some(Typle::Specific(Box::new(ty.clone())));
                                    continue;
                                }
                                PathArguments::Parenthesized(arguments) => {
                                    abort!(arguments, "parenthesized arguments not supported");
                                }
                            }
                        }
                    }
                }
            }
            bounds.push(bound);
        }
        Ok(result)
    }

    // Replace #[typle_attr_if(T::LEN == 1, unused_mut)]
    fn replace_attrs(&self, attrs: &mut Vec<Attribute>) -> syn::Result<()> {
        if attrs.iter().any(|attr| {
            if let Some(ident) = attr.path().get_ident() {
                ident == "typle_attr_if"
            } else {
                false
            }
        }) {
            for mut attr in std::mem::replace(attrs, Vec::with_capacity(attrs.len())) {
                if let (AttrStyle::Outer, Meta::List(meta_list)) = (&attr.style, &mut attr.meta) {
                    if let Some(ident) = meta_list.path.get_ident() {
                        if ident == "typle_attr_if" {
                            let mut tokens = std::mem::take(&mut meta_list.tokens).into_iter();
                            let expr_tokens = tokens
                                .by_ref()
                                .take_while(
                                    |tt| !matches!(tt, TokenTree::Punct(p) if p.as_char() == ','),
                                )
                                .collect();
                            let mut expr = syn::parse2::<Expr>(expr_tokens)?;
                            let mut state = BlockState::default();
                            self.replace_expr(&mut expr, &mut state)?;
                            if evaluate_bool(&expr)? {
                                meta_list.tokens = tokens.collect();
                                let nested = attr.parse_args_with(
                                    Punctuated::<Meta, token::Comma>::parse_terminated,
                                )?;
                                for meta in nested {
                                    attrs.push(Attribute {
                                        pound_token: attr.pound_token,
                                        style: attr.style,
                                        bracket_token: attr.bracket_token,
                                        meta,
                                    });
                                }
                            }
                            continue;
                        }
                    }
                }
                attrs.push(attr);
            }
        }
        Ok(())
    }

    fn replace_block(&self, block: &mut Block, state: &mut BlockState) -> syn::Result<()> {
        let stmts_len = block.stmts.len();
        let mut stmts = std::mem::replace(&mut block.stmts, Vec::with_capacity(stmts_len))
            .into_iter()
            .peekable();
        while let Some(stmt) = stmts.next() {
            match stmt {
                Stmt::Local(mut local) => {
                    self.replace_attrs(&mut local.attrs)?;
                    self.replace_pat(&mut local.pat, state)?;
                    if let Some(init) = &mut local.init {
                        self.replace_expr(&mut init.expr, state)?;
                        if let Some((_, diverge)) = &mut init.diverge {
                            self.replace_expr(diverge, state)?;
                        }
                    }
                    block.stmts.push(Stmt::Local(local));
                }
                Stmt::Item(item) => {
                    let mut items = Vec::new();
                    self.replace_item(item, &mut items)?;
                    block.stmts.extend(items.into_iter().map(Stmt::Item));
                }
                Stmt::Expr(mut expr, semi) => {
                    self.replace_expr(&mut expr, state)?;
                    // Remove empty blocks in blocks to allow control statements in const-for loop
                    // to be eliminated.
                    match &expr {
                        Expr::Block(syn::ExprBlock {
                            attrs,
                            label,
                            block: inner_block,
                        }) => {
                            if attrs.is_empty()
                                && label.is_none()
                                && inner_block.stmts.is_empty()
                                && (block.stmts.is_empty() || stmts.peek().is_some())
                            {
                                // Don't keep empty blocks inside blocks unless it affects the return value.
                                // i.e. only remove if there have been no statements before the empty block or
                                // if there are more statements afterwards.
                                continue;
                            }
                        }
                        Expr::Verbatim(_token_stream) => {
                            if block.stmts.is_empty() || stmts.peek().is_some() {
                                // Verbatim is only used to provide an empty block
                                continue;
                            }
                        }
                        _ => {}
                    }
                    block.stmts.push(Stmt::Expr(expr, semi));
                }
                Stmt::Macro(mut stmt_macro) => {
                    if let Some(stmt) =
                        self.replace_macro_expr(&mut stmt_macro.mac, &mut stmt_macro.attrs, state)?
                    {
                        block.stmts.push(Stmt::Expr(stmt, stmt_macro.semi_token));
                    } else {
                        block.stmts.push(Stmt::Macro(stmt_macro));
                    }
                }
            }
        }
        Ok(())
    }

    fn replace_fields(&self, fields: &mut Fields) -> syn::Result<()> {
        match fields {
            Fields::Named(syn::FieldsNamed { named: fields, .. })
            | Fields::Unnamed(syn::FieldsUnnamed {
                unnamed: fields, ..
            }) => {
                for field in fields {
                    self.replace_type(&mut field.ty)?;
                }
            }
            Fields::Unit => {}
        }
        Ok(())
    }

    pub fn replace_generics(&self, generics: &mut Generics) -> syn::Result<()> {
        if let Some(where_clause) = &mut generics.where_clause {
            for mut predicate in std::mem::take(&mut where_clause.predicates) {
                if let WherePredicate::Type(predicate_type) = &mut predicate {
                    match &mut predicate_type.bounded_ty {
                        Type::Path(type_path) => {
                            if let Some(first) = first_path_segment_mut(type_path) {
                                if let Some(Typle::Generic(component_names)) =
                                    self.typles.get(&first.ident)
                                {
                                    // T<{..}>: Copy, T<_>::Output: Copy
                                    // T: Copy and T<0>: Copy get replaced in the fallback
                                    if let PathArguments::AngleBracketed(arguments) =
                                        &first.arguments
                                    {
                                        let mut iter = arguments.args.iter().fuse();
                                        let (Some(arg), None) = (iter.next(), iter.next()) else {
                                            abort!(arguments, "expected constant expression");
                                        };
                                        let mut expr = match arg {
                                            GenericArgument::Type(Type::Infer(_)) => {
                                                let typle_len = self
                                                    .typle_len
                                                    .unwrap_or(self.typle_macro.max_len);
                                                syn::parse_quote!(.. #typle_len)
                                            }
                                            GenericArgument::Const(expr) => expr.clone(),
                                            _ => {
                                                abort!(arg, "expected const expression or `_`");
                                            }
                                        };
                                        let mut state = BlockState::default();
                                        self.replace_expr(&mut expr, &mut state)?;
                                        if let Some((start, end)) = evaluate_range(&expr) {
                                            let start = match start {
                                                Bound::Included(Err(span))
                                                | Bound::Excluded(Err(span)) => {
                                                    abort!(
                                                        span,
                                                        "expected integer for start of range"
                                                    );
                                                }
                                                Bound::Included(Ok(start)) => start,
                                                Bound::Excluded(Ok(start)) => {
                                                    start.saturating_add(1)
                                                }
                                                Bound::Unbounded => 0,
                                            };
                                            let end = match end {
                                                Bound::Included(Err(span))
                                                | Bound::Excluded(Err(span)) => {
                                                    abort!(
                                                        span,
                                                        "expected integer for end of range"
                                                    );
                                                }
                                                Bound::Included(Ok(end)) => end.saturating_add(1),
                                                Bound::Excluded(Ok(end)) => end,
                                                Bound::Unbounded => match self.typle_len {
                                                    Some(end) => end,
                                                    None => {
                                                        abort!(expr, "need an explicit range end");
                                                    }
                                                },
                                            };
                                            for bound in &mut predicate_type.bounds {
                                                if let TypeParamBound::Trait(trait_bound) = bound {
                                                    self.replace_path_arguments(
                                                        &mut trait_bound.path,
                                                    )?;
                                                }
                                            }
                                            for component_name in &component_names[start..end] {
                                                let mut type_path = type_path.clone();
                                                let first =
                                                    first_path_segment_mut(&mut type_path).unwrap();
                                                let component_ident =
                                                    Ident::new(component_name, first.ident.span());
                                                first.ident = component_ident;
                                                first.arguments = PathArguments::None;
                                                where_clause.predicates.push(WherePredicate::Type(
                                                    syn::PredicateType {
                                                        lifetimes: None,
                                                        bounded_ty: Type::Path(type_path),
                                                        colon_token: token::Colon::default(),
                                                        bounds: predicate_type.bounds.clone(),
                                                    },
                                                ));
                                            }
                                            continue;
                                        }
                                    }
                                }
                            }
                        }
                        Type::Macro(syn::TypeMacro { mac }) => {
                            if let Some(ident) = mac.path.get_ident() {
                                if ident == "typle" {
                                    let bounds = &predicate_type.bounds;
                                    if let (
                                        Some(TypeParamBound::Trait(syn::TraitBound {
                                            paren_token: None,
                                            modifier: syn::TraitBoundModifier::None,
                                            lifetimes: None,
                                            path:
                                                Path {
                                                    leading_colon: None,
                                                    segments,
                                                },
                                        })),
                                        None,
                                    ) = (bounds.get(0), bounds.get(1))
                                    {
                                        if let (
                                            Some(syn::PathSegment {
                                                ident: ident1,
                                                arguments: PathArguments::None,
                                            }),
                                            Some(syn::PathSegment {
                                                ident: ident2,
                                                arguments: PathArguments::None,
                                            }),
                                            None,
                                        ) = (segments.get(0), segments.get(1), segments.get(2))
                                        {
                                            if *ident1 == self.typle_macro.trait_ident
                                                && ident2 == "Bounds"
                                            {
                                                let token_stream = std::mem::take(&mut mac.tokens);
                                                let predicates =
                                                    self.expand_predicates(token_stream);
                                                for predicate in predicates {
                                                    where_clause.predicates.push(predicate?);
                                                }
                                                continue;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                    self.replace_type(&mut predicate_type.bounded_ty)?;
                    for bound in &mut predicate_type.bounds {
                        // substitute any appearances of typles in the constraints
                        // (e.g. T<_>: Extract<Output = S<0>::Output>)
                        self.replace_bound(bound)?;
                    }
                }
                where_clause.predicates.push(predicate);
            }
            if where_clause.predicates.is_empty() {
                generics.where_clause = None;
            }
        }
        for generic_param in std::mem::take(&mut generics.params) {
            match generic_param {
                GenericParam::Type(type_param) => match self.typles.get(&type_param.ident) {
                    Some(Typle::Generic(component_names)) => {
                        let typle_len = self.typle_len.unwrap_or(self.typle_macro.max_len);
                        for component_name in &component_names[..typle_len] {
                            let mut param = type_param.clone();
                            param.ident = Ident::new(component_name, type_param.ident.span());
                            generics.params.push(GenericParam::Type(param));
                        }
                    }
                    Some(Typle::Specific(_)) => {
                        // remove specific types from parameter list
                    }
                    None => {
                        generics.params.push(GenericParam::Type(type_param));
                    }
                },
                p => {
                    generics.params.push(p);
                }
            }
        }
        Ok(())
    }

    fn replace_bound(&self, bound: &mut TypeParamBound) -> syn::Result<()> {
        if let TypeParamBound::Trait(trait_bound) = bound {
            self.replace_path_arguments(&mut trait_bound.path)?;
        }
        Ok(())
    }

    fn expand_predicates(
        &self,
        token_stream: TokenStream,
    ) -> Replacements<impl Iterator<Item = syn::Result<WherePredicate>>> {
        self.expand_typle_macro(token_stream, |context, token_stream| {
            // Divide the token_stream at the first single :
            let mut bounded = Vec::new();
            let mut bound = Vec::new();
            let mut first_colon = None;
            for tt in token_stream {
                if bound.is_empty() {
                    match tt {
                        TokenTree::Punct(punct) if punct.as_char() == ':' => {
                            if let Some(colon) = first_colon.take() {
                                // ignore double-colon
                                bounded.push(TokenTree::Punct(colon));
                                bounded.push(TokenTree::Punct(punct));
                            } else {
                                // might be a single or double colon
                                first_colon = Some(punct);
                            }
                        }
                        tt => {
                            if first_colon.take().is_some() {
                                // first token of the bound
                                bound.push(tt);
                            } else {
                                bounded.push(tt);
                            }
                        }
                    }
                } else {
                    bound.push(tt);
                }
            }
            let mut bounded_ty = syn::parse2::<Type>(bounded.into_iter().collect())?;
            context.replace_type(&mut bounded_ty)?;
            let bounds = Punctuated::<TypeParamBound, token::Plus>::parse_terminated
                .parse2(bound.into_iter().collect())?
                .into_iter()
                .map(|mut bound| {
                    context.replace_bound(&mut bound)?;
                    Ok(bound)
                })
                .collect::<syn::Result<_>>()?;
            Ok(Replacements::<std::iter::Empty<_>>::Singleton(Ok(
                WherePredicate::Type(syn::PredicateType {
                    lifetimes: None,
                    bounded_ty,
                    colon_token: token::Colon::default(),
                    bounds,
                }),
            )))
        })
    }

    fn replace_generic_arguments(
        &self,
        args: &mut Punctuated<GenericArgument, token::Comma>,
    ) -> syn::Result<()> {
        for arg in std::mem::take(args) {
            match arg {
                GenericArgument::Type(Type::Path(syn::TypePath { qself, mut path }))
                    if qself.is_none() && path.leading_colon.is_none() =>
                {
                    let mut segments = path.segments.iter_mut();
                    if let Some(first) = segments.next() {
                        if let (Some(typle), PathArguments::AngleBracketed(arguments), None) = (
                            self.typles.get(&first.ident),
                            &mut first.arguments,
                            segments.next(),
                        ) {
                            let mut iter = arguments.args.iter_mut().fuse();
                            if let (Some(GenericArgument::Const(ref mut expr)), None) =
                                (iter.next(), iter.next())
                            {
                                let mut state = BlockState::default();
                                self.replace_expr(expr, &mut state)?;
                                if let Some((start, end)) = evaluate_range(expr) {
                                    // T<{..}>
                                    let start = match start {
                                        Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                            abort!(span, "expected integer for start of range");
                                        }
                                        Bound::Included(Ok(start)) => start,
                                        Bound::Excluded(Ok(start)) => start.saturating_add(1),
                                        Bound::Unbounded => 0,
                                    };
                                    let end = match end {
                                        Bound::Included(Err(span)) | Bound::Excluded(Err(span)) => {
                                            abort!(span, "expected integer for end of range");
                                        }
                                        Bound::Included(Ok(end)) => end.saturating_add(1),
                                        Bound::Excluded(Ok(end)) => end,
                                        Bound::Unbounded => match self.typle_len {
                                            Some(end) => end,
                                            None => {
                                                abort!(expr, "need an explicit range end");
                                            }
                                        },
                                    };
                                    for i in start..end {
                                        args.push(GenericArgument::Type(self.get_type(
                                            typle,
                                            i,
                                            path.span(),
                                        )?));
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    let mut generic_type = Type::Path(syn::TypePath { qself, path });
                    self.replace_type(&mut generic_type)?;
                    args.push(GenericArgument::Type(generic_type));
                }
                GenericArgument::Type(mut generic_type) => {
                    self.replace_type(&mut generic_type)?;
                    args.push(GenericArgument::Type(generic_type));
                }
                GenericArgument::Const(mut generic_expr) => {
                    let mut state = BlockState::default();
                    self.replace_expr(&mut generic_expr, &mut state)?;
                    if let Expr::Block(syn::ExprBlock { block, .. }) = &mut generic_expr {
                        if block.stmts.len() == 1 {
                            if let Stmt::Expr(expr, None) = &mut block.stmts[0] {
                                let e = std::mem::replace(expr, Expr::Verbatim(TokenStream::new()));
                                if let Expr::Lit(_) = e {
                                    // If we resolved <{T::LEN}> -> <{4}> then remove braces -> <4>
                                    generic_expr = e;
                                } else {
                                    // Put the expression back
                                    *expr = e;
                                }
                            }
                        }
                    }
                    args.push(GenericArgument::Const(generic_expr));
                }
                GenericArgument::AssocType(mut assoc_type) => {
                    if let Some(generic_args) = &mut assoc_type.generics {
                        self.replace_generic_arguments(&mut generic_args.args)?;
                    }
                    self.replace_type(&mut assoc_type.ty)?;
                    args.push(GenericArgument::AssocType(assoc_type));
                }
                GenericArgument::AssocConst(mut assoc_expr) => {
                    if let Some(generic_args) = &mut assoc_expr.generics {
                        self.replace_generic_arguments(&mut generic_args.args)?;
                    }
                    let mut state = BlockState::default();
                    self.replace_expr(&mut assoc_expr.value, &mut state)?;
                    args.push(GenericArgument::AssocConst(assoc_expr));
                }
                p => {
                    args.push(p);
                }
            }
        }
        Ok(())
    }

    pub fn replace_fn(
        &mut self,
        function: GeneralFunction,
        receiver_type: Option<&Type>,
        items: &mut Vec<Item>,
    ) -> syn::Result<GeneralFunction> {
        let fn_name = &function.sig.ident;
        let fn_meta = &function.attrs;
        let fn_vis = &function.vis;
        let fn_type_params_no_constraints = generic_param::remove_constraints(&function.sig);
        let fn_input_params = &function.sig.inputs;
        let mut type_tuple = syn::TypeTuple {
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        };
        let mut pat_tuple = syn::PatTuple {
            attrs: Vec::new(),
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        };
        let mut value_tuple = syn::ExprTuple {
            attrs: Vec::new(),
            paren_token: token::Paren::default(),
            elems: Punctuated::new(),
        };
        let fn_body = function.block;
        for arg in fn_input_params {
            match arg {
                FnArg::Receiver(r) => {
                    let Some(base_type) = receiver_type.cloned() else {
                        abort!(r, "did not expect receiver here");
                    };
                    let ty = if let Some((and_token, ref lifetime)) = r.reference {
                        Type::Reference(syn::TypeReference {
                            and_token,
                            lifetime: lifetime.clone(),
                            mutability: r.mutability,
                            elem: Box::new(base_type.clone()),
                        })
                    } else {
                        base_type.clone()
                    };
                    type_tuple.elems.push(ty);
                    pat_tuple.elems.push(Pat::Path(syn::PatPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: ident_to_path(Ident::new("_typle_self", r.span())),
                    }));
                    value_tuple.elems.push(Expr::Path(syn::PatPath {
                        attrs: Vec::new(),
                        qself: None,
                        path: ident_to_path(Ident::new("self", r.span())),
                    }));
                    self.renames
                        .insert(Ident::new("self", Span::call_site()), "_typle_self".into());
                    self.retypes
                        .insert(Ident::new("Self", Span::call_site()), base_type);
                }
                FnArg::Typed(pat_type) => {
                    type_tuple.elems.push(pat_type.ty.as_ref().clone());
                    pat_tuple.elems.push(pat_type.pat.as_ref().clone());
                    value_tuple
                        .elems
                        .push(pat_to_expr(pat_type.pat.as_ref().clone())?);
                }
            }
        }
        items.reserve(self.typle_macro.max_len - self.typle_macro.min_len + 2);
        // A trait with an apply method to implement for each tuple
        let trait_name = format_ident!("_typle_fn_{}", fn_name);
        let trait_item = syn::parse_quote!(
            #[allow(non_camel_case_types)]
            #fn_vis trait #trait_name {
                type Return;

                fn apply(self) -> Self::Return;
            }
        );
        items.push(trait_item);

        // A function that turns the function argument list into a tuple that
        // implements the trait and calls the apply method from the trait.
        let fn_item = GeneralFunction {
            attrs: Vec::new(),
            vis: fn_vis.clone(),
            sig: syn::parse_quote!(
            fn #fn_name <#fn_type_params_no_constraints>(#fn_input_params) -> <#type_tuple as #trait_name>::Return
            where
                #type_tuple: #trait_name
            ),
            block: syn::parse_quote!({
                <#type_tuple as #trait_name>::apply(#value_tuple)
            }),
        };

        let return_type = match function.sig.output {
            ReturnType::Default => Type::Tuple(syn::TypeTuple {
                paren_token: token::Paren::default(),
                elems: Punctuated::new(),
            }),
            ReturnType::Type(_, t) => *t,
        };
        let typle_trait_name = &self.typle_macro.trait_ident;

        // A method body is moved to a trait implementation on a dfferent type.
        // Any instances of `Self` in the method body are converted to the
        // actual self type. Instances of `self` are converted to `_typle_self`.
        // To get `Self` and `self` in the right places we convert any instances
        // of `_typle_Self` and `_typle_self` to `Self` and `self`.
        self.retypes.insert(
            Ident::new("_typle_Self", Span::call_site()),
            Type::Path(syn::TypePath {
                qself: None,
                path: ident_to_path(Ident::new("Self", Span::call_site())),
            }),
        );
        self.renames
            .insert(Ident::new("_typle_self", Span::call_site()), "self".into());

        let impl_items = vec![
            ImplItem::Type(syn::ImplItemType {
                attrs: Vec::new(),
                vis: Visibility::Inherited,
                defaultness: None,
                type_token: token::Type::default(),
                ident: Ident::new("Return", Span::call_site()),
                generics: Generics::default(),
                eq_token: token::Eq::default(),
                ty: return_type,
                semi_token: token::Semi::default(),
            }),
            syn::parse_quote!(
                #(#fn_meta)*
                fn apply(self) -> _typle_Self::Return {
                    #[typle_attr_if(#typle_trait_name::LEN == 0, allow(unused_variables))]
                    let #pat_tuple = _typle_self;
                    #fn_body
                }
            ),
        ];

        let item = syn::ItemImpl {
            attrs: Vec::new(),
            defaultness: None,
            unsafety: None,
            impl_token: token::Impl::default(),
            generics: function.sig.generics,
            trait_: Some((None, ident_to_path(trait_name), token::For::default())),
            self_ty: Box::new(Type::Tuple(type_tuple)),
            brace_token: token::Brace::default(),
            items: impl_items,
        };

        for (typle_len, item) in
            (self.typle_macro.min_len..=self.typle_macro.max_len).zip_clone(item)
        {
            self.typle_len = Some(typle_len);
            self.replace_item_impl(item, items)?;
        }
        Ok(fn_item)
    }

    pub fn replace_item(&self, item: Item, items: &mut Vec<Item>) -> syn::Result<()> {
        match item {
            Item::Const(mut constant) => {
                let context = self.extract_typle_constraints(&mut constant.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut constant.attrs)?;
                context.replace_type(&mut constant.ty)?;
                let mut state = BlockState::default();
                context.replace_expr(&mut constant.expr, &mut state)?;
                items.push(Item::Const(constant));
            }
            Item::Enum(mut enum_item) => {
                let context = self.extract_typle_constraints(&mut enum_item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut enum_item.attrs)?;
                context.replace_generics(&mut enum_item.generics)?;
                for mut variant in std::mem::take(&mut enum_item.variants) {
                    if let Fields::Unit = variant.fields {
                        if let Some((_, Expr::Macro(r#macro))) = &mut variant.discriminant {
                            if let Some(ident) = r#macro.mac.path.get_ident() {
                                if ident == "typle_variant" {
                                    let token_stream = std::mem::take(&mut r#macro.mac.tokens);
                                    let mut tokens = token_stream.into_iter();
                                    let (pattern, range) =
                                        context.parse_pattern_range(&mut tokens)?;
                                    if range.is_empty() {
                                        continue;
                                    }
                                    let token_stream = tokens.collect();
                                    let fields = match r#macro.mac.delimiter {
                                        MacroDelimiter::Paren(_) => {
                                            let group = TokenTree::Group(Group::new(
                                                proc_macro2::Delimiter::Parenthesis,
                                                token_stream,
                                            ));
                                            Fields::Unnamed(syn::parse2::<syn::FieldsUnnamed>(
                                                TokenStream::from(group),
                                            )?)
                                        }
                                        MacroDelimiter::Brace(_) => {
                                            let group = TokenTree::Group(Group::new(
                                                proc_macro2::Delimiter::Brace,
                                                token_stream,
                                            ));
                                            Fields::Named(syn::parse2::<syn::FieldsNamed>(
                                                TokenStream::from(group),
                                            )?)
                                        }
                                        MacroDelimiter::Bracket(_) => {
                                            if !token_stream.is_empty() {
                                                abort!(token_stream, "braces require empty body");
                                            }
                                            Fields::Unit
                                        }
                                    };
                                    let mut context = context.clone();
                                    if let Some(ident) = &pattern {
                                        context.constants.insert(ident.clone(), 0);
                                    }
                                    for (index, mut fields) in range.zip_clone(fields) {
                                        if let Some(ident) = &pattern {
                                            *context.constants.get_mut(ident).unwrap() = index;
                                        }
                                        match &mut fields {
                                            Fields::Named(syn::FieldsNamed {
                                                named: fields,
                                                ..
                                            })
                                            | Fields::Unnamed(syn::FieldsUnnamed {
                                                unnamed: fields,
                                                ..
                                            }) => {
                                                for field in fields {
                                                    context.replace_type(&mut field.ty)?;
                                                }
                                            }
                                            Fields::Unit => {}
                                        }
                                        let variant = syn::Variant {
                                            attrs: variant.attrs.clone(),
                                            ident: format_ident!("{}{}", &variant.ident, index),
                                            fields,
                                            discriminant: None,
                                        };
                                        enum_item.variants.push(variant);
                                    }
                                    continue;
                                }
                            }
                        }
                    }
                    context.replace_fields(&mut variant.fields)?;
                    if let Some((_, discriminant)) = &mut variant.discriminant {
                        let mut state = BlockState::default();
                        context.replace_expr(discriminant, &mut state)?;
                    }
                    enum_item.variants.push(variant);
                }
                items.push(Item::Enum(enum_item));
            }
            Item::Fn(mut function) => {
                if self.typle_len.is_none() {
                    if let Some(mut context) =
                        self.extract_typle_constraints(&mut function.sig.generics)?
                    {
                        let fn_item = context.replace_fn(function.into(), None, items)?;
                        items.push(Item::Fn(fn_item.into()));
                        return Ok(());
                    }
                }
                let context = self.extract_typle_constraints(&mut function.sig.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut function.attrs)?;
                context.replace_signature(&mut function.sig)?;
                let mut state = BlockState::default();
                context.replace_block(&mut function.block, &mut state)?;
                items.push(Item::Fn(function));
            }
            Item::Impl(mut item) => {
                if self.typle_len.is_none() {
                    if let Some(mut context) = self.extract_typle_constraints(&mut item.generics)? {
                        items.reserve(self.typle_macro.max_len - self.typle_macro.min_len + 1);
                        for (typle_len, item) in
                            (self.typle_macro.min_len..=self.typle_macro.max_len).zip_clone(item)
                        {
                            context.typle_len = Some(typle_len);
                            context.replace_item_impl(item, items)?;
                        }
                        return Ok(());
                    }
                }
                let context = self.extract_typle_constraints(&mut item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_item_impl(item, items)?;
            }
            Item::Macro(mut item) => {
                self.replace_attrs(&mut item.attrs)?;
                item.mac.tokens =
                    self.replace_macro_token_stream(std::mem::take(&mut item.mac.tokens))?;
                items.push(Item::Macro(item));
            }
            Item::Mod(mut module) => {
                if let Some((_, inner_items)) = &mut module.content {
                    for item in
                        std::mem::replace(inner_items, Vec::with_capacity(inner_items.len()))
                    {
                        self.replace_item(item, inner_items)?;
                    }
                }
                items.push(Item::Mod(module));
            }
            Item::Struct(mut struct_item) => {
                let context = self.extract_typle_constraints(&mut struct_item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut struct_item.attrs)?;
                context.replace_generics(&mut struct_item.generics)?;
                context.replace_fields(&mut struct_item.fields)?;
                items.push(Item::Struct(struct_item));
            }
            Item::Type(mut type_item) => {
                let context = self.extract_typle_constraints(&mut type_item.generics)?;
                let context = context.as_ref().unwrap_or(self);
                context.replace_attrs(&mut type_item.attrs)?;
                context.replace_generics(&mut type_item.generics)?;
                context.replace_type(&mut type_item.ty)?;
                items.push(Item::Type(type_item));
            }
            item => {
                items.push(item);
            }
        }
        Ok(())
    }

    fn replace_item_impl(&self, mut item: syn::ItemImpl, items: &mut Vec<Item>) -> syn::Result<()> {
        self.replace_attrs(&mut item.attrs)?;
        self.replace_generics(&mut item.generics)?;
        if let Some((_, path, _)) = &mut item.trait_ {
            self.replace_path_arguments(path)?;
        }
        self.replace_type(&mut item.self_ty)?;
        for subitem in &mut item.items {
            match subitem {
                ImplItem::Const(constant) => {
                    self.replace_attrs(&mut constant.attrs)?;
                    self.replace_type(&mut constant.ty)?;
                    let mut state = BlockState::default();
                    self.replace_expr(&mut constant.expr, &mut state)?;
                }
                ImplItem::Fn(function) => {
                    if self.typle_len.is_none() {
                        if let Some(mut context) =
                            self.extract_typle_constraints(&mut function.sig.generics)?
                        {
                            *function = context
                                .replace_fn(function.clone().into(), Some(&item.self_ty), items)?
                                .into();
                            continue;
                        }
                    }
                    self.replace_attrs(&mut function.attrs)?;
                    let inner_context =
                        self.extract_typle_constraints(&mut function.sig.generics)?;
                    let inner_context = inner_context.as_ref().unwrap_or(self);
                    inner_context.replace_signature(&mut function.sig)?;
                    let mut state = BlockState::default();
                    inner_context.replace_block(&mut function.block, &mut state)?;
                }
                ImplItem::Type(ty) => {
                    self.replace_attrs(&mut ty.attrs)?;
                    self.replace_type(&mut ty.ty)?;
                }
                ImplItem::Macro(subitem) => {
                    self.replace_attrs(&mut subitem.attrs)?;
                    subitem.mac.tokens =
                        self.replace_macro_token_stream(std::mem::take(&mut subitem.mac.tokens))?;
                }
                _ => {}
            }
        }
        items.push(Item::Impl(item));
        Ok(())
    }

    fn replace_typle_for_expr<'a>(
        &'a self,
        mac: &mut Macro,
        state: &'a mut BlockState,
    ) -> syn::Result<impl Iterator<Item = syn::Result<Expr>> + 'a> {
        let token_stream = std::mem::take(&mut mac.tokens);
        let mut tokens = token_stream.into_iter();
        let (pattern, range) = self.parse_pattern_range(&mut tokens)?;
        let token_stream = tokens.collect::<TokenStream>();
        let mut context = self.clone();
        if let Some(ident) = &pattern {
            context.constants.insert(ident.clone(), 0);
        }
        Ok(range
            .zip_clone(token_stream)
            .flat_map(move |(index, token_stream)| {
                if let Some(ident) = &pattern {
                    *context.constants.get_mut(ident).unwrap() = index;
                }
                let token_stream = match context.evaluate_if(token_stream) {
                    Ok(Some(token_stream)) => token_stream,
                    Ok(None) => {
                        return None;
                    }
                    Err(e) => {
                        return Some(Err(e));
                    }
                };
                let mut expr = match syn::parse2::<Expr>(token_stream) {
                    Ok(expr) => expr,
                    Err(e) => return Some(Err(e)),
                };
                match context.replace_expr(&mut expr, state) {
                    Ok(()) => {
                        if let Expr::Verbatim(_token_stream) = expr {
                            // Verbatim causes typle to omit the component from the tuple
                            None
                        } else {
                            Some(Ok(expr))
                        }
                    }
                    Err(error) => Some(Err(error)),
                }
            }))
    }

    fn replace_typle_fold_expr(
        &self,
        mac: &mut Macro,
        attrs: Vec<Attribute>,
        state: &mut BlockState,
        default_span: Span,
    ) -> syn::Result<Expr> {
        let mut inner_state = BlockState::default();
        let token_stream = std::mem::take(&mut mac.tokens);
        let mut tokens = token_stream.into_iter();
        let mut init_expr =
            syn::parse2::<Expr>(Self::extract_to_semicolon(&mut tokens, default_span)?)?;
        self.replace_expr(&mut init_expr, &mut inner_state)?;
        let (pattern, range) = self.parse_pattern_range(&mut tokens)?;
        if range.is_empty() {
            return Ok(Expr::Paren(syn::ExprParen {
                attrs,
                paren_token: token::Paren::default(),
                expr: Box::new(init_expr),
            }));
        }
        let fold_ident = Self::parse_fold_ident(&mut tokens, default_span)?;
        let fold_pat = Pat::Ident(syn::PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: fold_ident.clone(),
            subpat: None,
        });
        let expr = syn::parse2::<Expr>(tokens.collect())?;
        let mut stmts = Vec::with_capacity(range.len() + 2);
        stmts.push(Stmt::Local(syn::Local {
            attrs: Vec::new(),
            let_token: token::Let::default(),
            pat: fold_pat.clone(),
            init: Some(syn::LocalInit {
                eq_token: token::Eq::default(),
                expr: Box::new(init_expr),
                diverge: None,
            }),
            semi_token: token::Semi::default(),
        }));
        if !range.is_empty() {
            let mut ctx = pattern.as_ref().map(|ident| {
                let mut context = self.clone();
                context.constants.insert(ident.clone(), 0);
                (context, ident)
            });
            for (index, mut expr) in range.zip_clone(expr) {
                if let Some((ref mut context, ident)) = ctx {
                    *context.constants.get_mut(ident).unwrap() = index;
                }
                let context = ctx.as_ref().map_or(self, |c| &c.0);
                context.replace_expr(&mut expr, &mut inner_state)?;
                stmts.push(Stmt::Local(syn::Local {
                    attrs: Vec::new(),
                    let_token: token::Let::default(),
                    pat: fold_pat.clone(),
                    init: Some(syn::LocalInit {
                        eq_token: token::Eq::default(),
                        expr: Box::new(expr),
                        diverge: None,
                    }),
                    semi_token: token::Semi::default(),
                }));
            }
        }
        if let Some(span) = inner_state.unlabelled_continue {
            abort!(
                span,
                "unlabelled `continue` not supported in `typle_fold!` macro"
            );
        }
        state.propagate(inner_state, None);
        stmts.push(Stmt::Expr(
            Expr::Break(syn::ExprBreak {
                attrs: Vec::new(),
                break_token: token::Break::default(),
                label: None,
                expr: Some(Box::new(Expr::Path(syn::PatPath {
                    attrs: Vec::new(),
                    qself: None,
                    path: ident_to_path(fold_ident),
                }))),
            }),
            Some(token::Semi::default()),
        ));
        // The fold may be used in an expression `typle_fold!() + ...`, in which
        // case the loop needs to be in parentheses.
        Ok(Expr::Paren(syn::ExprParen {
            attrs,
            paren_token: token::Paren::default(),
            expr: Box::new(Expr::Loop(syn::ExprLoop {
                attrs: Vec::new(),
                label: None,
                loop_token: token::Loop::default(),
                body: Block {
                    brace_token: token::Brace::default(),
                    stmts,
                },
            })),
        }))
    }

    fn replace_macro_expr(
        &self,
        mac: &mut Macro,
        attrs: &mut Vec<Attribute>,
        state: &mut BlockState,
    ) -> syn::Result<Option<Expr>> {
        // typle_for!(i in .. => Some<t[[i]]) -> (Some<t.0>, Some<t.1>)
        // typle_for!(i in .. => t.to_string()) -> (t.0.to_string(), t.1.to_string())
        // as opposed to
        // Some(t) -> Some((t.0, t.1))
        // t.to_string() -> (t.0, t.1).to_string()
        self.replace_attrs(attrs)?;
        if let Some(macro_name) = mac.path.get_ident() {
            let default_span = macro_name.span();
            if macro_name == "typle_for" {
                let expr = {
                    let elems = self.replace_typle_for_expr(mac, state)?;
                    let tuple = syn::ExprTuple {
                        attrs: std::mem::take(attrs),
                        paren_token: token::Paren::default(),
                        elems: elems.collect::<syn::Result<_>>()?,
                    };
                    Expr::Tuple(tuple)
                };
                return Ok(Some(expr));
            } else if macro_name == "typle_fold" {
                let expr =
                    self.replace_typle_fold_expr(mac, std::mem::take(attrs), state, default_span)?;
                return Ok(Some(expr));
            } else if macro_name == "typle_all" {
                let token_stream = std::mem::take(&mut mac.tokens);
                let expr = self.anyall(
                    token_stream,
                    state,
                    BinOp::And(token::AndAnd::default()),
                    true,
                )?;
                return Ok(Some(expr));
            } else if macro_name == "typle_any" {
                let token_stream = std::mem::take(&mut mac.tokens);
                let expr = self.anyall(
                    token_stream,
                    state,
                    BinOp::Or(token::OrOr::default()),
                    false,
                )?;
                return Ok(Some(expr));
            }
        }
        mac.tokens = self.replace_macro_token_stream(std::mem::take(&mut mac.tokens))?;
        Ok(None)
    }

    fn anyall(
        &self,
        token_stream: TokenStream,
        state: &mut BlockState,
        op: BinOp,
        default: bool,
    ) -> syn::Result<Expr> {
        let mut tokens = token_stream.into_iter();
        let (pattern, mut range) = self.parse_pattern_range(&mut tokens)?;
        let expr = syn::parse2::<Expr>(tokens.collect())?;
        let all = match range.next() {
            Some(index) => {
                // Parenthesize low precedence expressions.
                let expr = match expr {
                    expr @ Expr::Lit(syn::PatLit {
                        lit: Lit::Bool(_), ..
                    }) => expr,
                    expr @ Expr::Block(_) => expr,
                    expr @ Expr::Paren(_) => expr,
                    expr @ Expr::Path(_) => expr,
                    expr @ Expr::MethodCall(_) => expr,
                    expr @ Expr::Field(_) => expr,
                    expr @ Expr::Call(_) => expr,
                    expr @ Expr::Index(_) => expr,
                    expr @ Expr::Unary(_) => expr,
                    expr @ Expr::Cast(_) => expr,
                    expr => Expr::Paren(syn::ExprParen {
                        attrs: Vec::new(),
                        paren_token: token::Paren::default(),
                        expr: Box::new(expr),
                    }),
                };

                let mut context = self.clone();
                if let Some(ident) = &pattern {
                    context.constants.insert(ident.clone(), index);
                }
                let mut all = expr.clone();
                context.replace_expr(&mut all, state)?;
                for (index, mut expr) in range.zip_clone(expr) {
                    if let Some(ident) = &pattern {
                        *context.constants.get_mut(ident).unwrap() = index;
                    }
                    context.replace_expr(&mut expr, state)?;
                    all = Expr::Binary(syn::ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(all),
                        op,
                        right: Box::new(expr),
                    });
                }
                if let Expr::Binary(expr) = all {
                    // Wrap entire expression in parentheses to:
                    // 1. ensure that it is a single expression in a larger expression.
                    // 2. handle ambiguity: `{ true } || { true }` is not a boolean expression.
                    // It is a block followed by a closure that returns a boolean. Adding
                    // parentheses `({ true } || { true })` makes it a boolean expression.
                    all = Expr::Paren(syn::ExprParen {
                        attrs: Vec::new(),
                        paren_token: token::Paren::default(),
                        expr: Box::new(Expr::Binary(expr)),
                    });
                }
                all
            }
            None => Expr::Lit(syn::PatLit {
                attrs: Vec::new(),
                lit: Lit::Bool(syn::LitBool {
                    value: default,
                    span: expr.span(),
                }),
            }),
        };
        Ok(all)
    }

    // Look for `typle_ty!(...)` or `typle_expr!(...)` and evaluate body.
    fn replace_macro_token_stream(&self, input: TokenStream) -> syn::Result<TokenStream> {
        enum TTState {
            Start,
            FoundIdent(Ident),
            TypleTy,
            TypleExpr,
        }

        let mut output = TokenStream::new();
        let mut state = TTState::Start;
        for tt in input {
            match tt {
                TokenTree::Group(group) => {
                    match state {
                        TTState::Start => {
                            output.extend([TokenTree::Group(Group::new(
                                group.delimiter(),
                                self.replace_macro_token_stream(group.stream())?,
                            ))]);
                        }
                        TTState::FoundIdent(ident) => {
                            output.extend([
                                TokenTree::Ident(ident),
                                TokenTree::Group(Group::new(
                                    group.delimiter(),
                                    self.replace_macro_token_stream(group.stream())?,
                                )),
                            ]);
                        }
                        TTState::TypleTy => {
                            let mut ty = syn::parse2(group.stream())?;
                            self.replace_type(&mut ty)?;
                            output.extend(ty.into_token_stream());
                        }
                        TTState::TypleExpr => {
                            let mut expr = syn::parse2(group.stream())?;
                            let mut state = BlockState::default();
                            self.replace_expr(&mut expr, &mut state)?;
                            output.extend(expr.into_token_stream());
                        }
                    }
                    state = TTState::Start;
                }
                TokenTree::Ident(ident) => {
                    match state {
                        TTState::Start => {}
                        TTState::FoundIdent(prev) => {
                            output.extend([TokenTree::Ident(prev)]);
                        }
                        TTState::TypleTy | TTState::TypleExpr => {
                            abort!(ident, "typle: expected macro body");
                        }
                    }
                    state = TTState::FoundIdent(ident);
                }
                TokenTree::Punct(punct) => {
                    match state {
                        TTState::Start => {}
                        TTState::FoundIdent(ident) => {
                            if punct.as_char() == '!' {
                                if ident == "typle_ty" {
                                    state = TTState::TypleTy;
                                    continue;
                                } else if ident == "typle_expr" {
                                    state = TTState::TypleExpr;
                                    continue;
                                }
                            }
                            output.extend([TokenTree::Ident(ident)]);
                        }
                        TTState::TypleTy | TTState::TypleExpr => {
                            abort!(punct, "typle: expected macro body");
                        }
                    }
                    output.extend([TokenTree::Punct(punct)]);
                    state = TTState::Start;
                }
                TokenTree::Literal(literal) => {
                    match state {
                        TTState::Start => {}
                        TTState::FoundIdent(ident) => {
                            output.extend([TokenTree::Ident(ident)]);
                        }
                        TTState::TypleTy | TTState::TypleExpr => {
                            abort!(literal, "typle: expected macro body");
                        }
                    }
                    output.extend([TokenTree::Literal(literal)]);
                    state = TTState::Start;
                }
            }
        }
        match state {
            TTState::Start => {}
            TTState::FoundIdent(ident) => {
                output.extend([TokenTree::Ident(ident)]);
            }
            TTState::TypleTy | TTState::TypleExpr => {
                return Err(syn::Error::new(
                    Span::call_site(),
                    "typle: expected macro body",
                ));
            }
        }
        Ok(output)
    }

    fn extract_to_semicolon(
        tokens: &mut impl Iterator<Item = TokenTree>,
        span: Span,
    ) -> syn::Result<TokenStream> {
        let mut collect = TokenStream::new();
        for token in tokens.by_ref() {
            match token {
                TokenTree::Punct(punct) if punct.as_char() == ';' => {
                    return Ok(collect);
                }
                tt => {
                    collect.extend([tt]);
                }
            }
        }
        Err(syn::Error::new(
            span,
            "typle expected tokens terminated by ;",
        ))
    }

    fn parse_fold_ident(
        tokens: &mut impl Iterator<Item = TokenTree>,
        span: Span,
    ) -> syn::Result<Ident> {
        let Some(TokenTree::Punct(punct)) = tokens.next() else {
            return Err(syn::Error::new(span, "expected `=> |accumulator|`"));
        };
        if punct.as_char() != '|' {
            return Err(syn::Error::new(span, "expected `=> |accumulator|`"));
        }
        let Some(TokenTree::Ident(mut ident)) = tokens.next() else {
            return Err(syn::Error::new(span, "expected `=> |accumulator|`"));
        };
        let Some(TokenTree::Punct(punct)) = tokens.next() else {
            return Err(syn::Error::new(span, "expected `=> |accumulator|`"));
        };
        if punct.as_char() != '|' {
            return Err(syn::Error::new(span, "expected `=> |accumulator|`"));
        }
        if ident == "_" {
            ident = Ident::new("_typle", ident.span());
        }
        Ok(ident)
    }

    fn replace_path_arguments(&self, path: &mut Path) -> syn::Result<()> {
        for mut path_segment in std::mem::take(&mut path.segments) {
            match &mut path_segment.arguments {
                PathArguments::None => {}
                PathArguments::AngleBracketed(args) => {
                    if let Some(index) = self.typle_ident(args)? {
                        // X::<typle_ident!(3)> -> X3
                        path_segment.ident = format_ident!("{}{}", path_segment.ident, index);
                        path_segment.arguments = PathArguments::None;
                    } else {
                        // WrapperType<T> -> WrapperType<(T0, T1,...)>
                        // WrapperType<T<3>> -> WrapperType<T3>
                        // WrapperType<T<{..}>> -> WrapperType<T0, T1,...>
                        self.replace_generic_arguments(&mut args.args)?;
                        if args.args.is_empty() {
                            path_segment.arguments = PathArguments::None;
                        }
                    }
                }
                PathArguments::Parenthesized(args) => {
                    args.inputs = std::mem::take(&mut args.inputs)
                        .into_iter()
                        .flat_map(|ty| self.replace_type_in_list(ty))
                        .collect::<syn::Result<_>>()?;
                    if let ReturnType::Type(_, ty) = &mut args.output {
                        self.replace_type(ty)?;
                    }
                }
            }
            path.segments.push(path_segment);
        }
        Ok(())
    }

    fn replace_signature(&self, sig: &mut Signature) -> syn::Result<()> {
        self.replace_generics(&mut sig.generics)?;
        for input in &mut sig.inputs {
            if let FnArg::Typed(pat_type) = input {
                self.replace_type(&mut pat_type.ty)?;
            }
        }
        match &mut sig.output {
            ReturnType::Default => {}
            ReturnType::Type(_, ty) => {
                self.replace_type(ty)?;
            }
        }
        Ok(())
    }

    // Return Some(usize) if the angled bracketed generic arguments is a `typle_ident!` macro.
    fn typle_ident(
        &self,
        args: &mut syn::AngleBracketedGenericArguments,
    ) -> syn::Result<Option<usize>> {
        if args.args.len() == 1 {
            if let Some(GenericArgument::Type(Type::Macro(syn::TypeMacro { mac }))) =
                args.args.first_mut()
            {
                if let Some(macro_ident) = mac.path.get_ident() {
                    if macro_ident == "typle_ident" {
                        let mut expr = syn::parse2::<Expr>(std::mem::take(&mut mac.tokens))?;
                        let mut state = BlockState::default();
                        self.replace_expr(&mut expr, &mut state)?;
                        let Some(index) = evaluate_usize(&expr) else {
                            abort!(mac.tokens, "expect constant integer");
                        };
                        return Ok(Some(index));
                    }
                }
            }
        }
        Ok(None)
    }
}

#[inline]
pub fn ident_to_path(ident: Ident) -> Path {
    Path {
        leading_colon: None,
        segments: std::iter::once(syn::PathSegment {
            ident,
            arguments: PathArguments::None,
        })
        .collect(),
    }
}

fn pat_to_expr(pat: Pat) -> syn::Result<Expr> {
    match pat {
        Pat::Const(p) => Ok(Expr::Const(p)),
        Pat::Ident(p) => Ok(Expr::Path(syn::PatPath {
            attrs: p.attrs,
            qself: None,
            path: ident_to_path(p.ident),
        })),
        Pat::Lit(p) => Ok(Expr::Lit(p)),
        Pat::Macro(p) => Ok(Expr::Macro(p)),
        Pat::Tuple(p) => Ok(Expr::Tuple(syn::ExprTuple {
            attrs: p.attrs,
            paren_token: p.paren_token,
            elems: p
                .elems
                .into_iter()
                .map(pat_to_expr)
                .collect::<syn::Result<_>>()?,
        })),
        _ => Err(syn::Error::new(pat.span(), "unsupported pattern")),
    }
}

pub fn first_path_segment_mut(mut type_path: &mut syn::TypePath) -> Option<&mut syn::PathSegment> {
    while let Some(qself) = &mut type_path.qself {
        // <<T<_> as Trait>::Value as std::ops::Mul>::Output: Copy
        if let Type::Path(inner_type_path) = &mut *qself.ty {
            type_path = inner_type_path;
        } else {
            return None;
        }
    }
    type_path.path.segments.first_mut()
}
