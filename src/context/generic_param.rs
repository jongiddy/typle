use super::*;

#[derive(Default)]
struct GenericParameterMap<'a> {
    lifetimes: HashMap<&'a Ident, &'a LifetimeParam>,
    named: HashMap<&'a Ident, &'a GenericParam>,
}

// Remove any bounds on a signature's generic parameters. Return a list of generic parameters that
// are still required. e.g. `fn f<'a, C, T: Tuple<C>>(t: &'a T)` becomes `fn f<'a, T>(t: &'a T)`.
pub(super) fn remove_constraints(sig: &Signature) -> Punctuated<GenericParam, token::Comma> {
    let mut output = Punctuated::new();
    let mut param_map = GenericParameterMap::default();
    for param in &sig.generics.params {
        match param {
            GenericParam::Lifetime(lifetime_param) => {
                param_map
                    .lifetimes
                    .insert(&lifetime_param.lifetime.ident, lifetime_param);
            }
            GenericParam::Type(type_param) => {
                param_map.named.insert(&type_param.ident, param);
            }
            GenericParam::Const(const_param) => {
                param_map.named.insert(&const_param.ident, param);
            }
        }
    }
    for input in &sig.inputs {
        match input {
            FnArg::Receiver(receiver) => {
                if let Some(lifetime) = receiver
                    .reference
                    .as_ref()
                    .and_then(|(_, lifetime)| lifetime.as_ref())
                {
                    if let Some(param) = param_map.lifetimes.remove(&lifetime.ident) {
                        output.push(GenericParam::Lifetime(param.clone()));
                    }
                }
            }
            FnArg::Typed(pat_type) => {
                handle_type(&pat_type.ty, &mut param_map, &mut output);
            }
        }
    }
    output
}

fn handle_type(
    ty: &Type,
    param_map: &mut GenericParameterMap,
    output: &mut Punctuated<GenericParam, token::Comma>,
) {
    match ty {
        Type::Array(type_array) => {
            handle_type(&type_array.elem, param_map, output);
        }
        Type::BareFn(_type_bare_fn) => {}
        Type::Group(type_group) => {
            handle_type(&type_group.elem, param_map, output);
        }
        Type::ImplTrait(_type_impl_trait) => {}
        Type::Infer(_type_infer) => {}
        Type::Macro(_type_macro) => {}
        Type::Never(_type_never) => {}
        Type::Paren(type_paren) => {
            handle_type(&type_paren.elem, param_map, output);
        }
        Type::Path(type_path) => {
            if let Some(ident) = type_path.path.get_ident() {
                if let Some(param) = param_map.named.remove(ident) {
                    match param {
                        GenericParam::Lifetime(_) => unreachable!(),
                        GenericParam::Type(type_param) => {
                            output.push(GenericParam::Type(TypeParam {
                                attrs: vec![],
                                ident: type_param.ident.clone(),
                                colon_token: None,
                                bounds: Punctuated::new(),
                                eq_token: None,
                                default: None,
                            }))
                        }
                        GenericParam::Const(_) => {
                            output.push(param.clone());
                        }
                    }
                }
            }
        }
        Type::Ptr(type_ptr) => {
            handle_type(&type_ptr.elem, param_map, output);
        }
        Type::Reference(type_reference) => {
            if let Some(lifetime) = &type_reference.lifetime {
                if let Some(param) = param_map.lifetimes.remove(&lifetime.ident) {
                    output.push(GenericParam::Lifetime(param.clone()))
                }
            }
            handle_type(&type_reference.elem, param_map, output);
        }
        Type::Slice(type_slice) => {
            handle_type(&type_slice.elem, param_map, output);
        }
        Type::TraitObject(_type_trait_object) => {}
        Type::Tuple(type_tuple) => {
            for elem in &type_tuple.elems {
                handle_type(elem, param_map, output);
            }
        }
        Type::Verbatim(_token_stream) => {}
        _ => {}
    }
}
