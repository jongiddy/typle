use syn::{Attribute, Block, ImplItemFn, ItemFn, Signature, Visibility};

pub struct GeneralFunction {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub sig: Signature,
    pub block: Block,
}

impl From<ItemFn> for GeneralFunction {
    fn from(source: ItemFn) -> Self {
        GeneralFunction {
            attrs: source.attrs,
            vis: source.vis,
            sig: source.sig,
            block: *source.block,
        }
    }
}

impl From<GeneralFunction> for ItemFn {
    fn from(source: GeneralFunction) -> Self {
        ItemFn {
            attrs: source.attrs,
            vis: source.vis,
            sig: source.sig,
            block: Box::new(source.block),
        }
    }
}

impl From<ImplItemFn> for GeneralFunction {
    fn from(source: ImplItemFn) -> Self {
        GeneralFunction {
            attrs: source.attrs,
            vis: source.vis,
            sig: source.sig,
            block: source.block,
        }
    }
}

impl From<GeneralFunction> for ImplItemFn {
    fn from(source: GeneralFunction) -> Self {
        ImplItemFn {
            attrs: source.attrs,
            vis: source.vis,
            defaultness: None,
            sig: source.sig,
            block: source.block,
        }
    }
}
