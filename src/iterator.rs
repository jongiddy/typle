pub enum ListIterator4<I0, I1, I2, I3> {
    Variant0(I0),
    Variant1(I1),
    Variant2(I2),
    Variant3(I3),
}

impl<I0, I1, I2, I3> Iterator for ListIterator4<I0, I1, I2, I3>
where
    I0: Iterator,
    I1: Iterator<Item = I0::Item>,
    I2: Iterator<Item = I0::Item>,
    I3: Iterator<Item = I0::Item>,
{
    type Item = I0::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ListIterator4::Variant0(iter) => iter.next(),
            ListIterator4::Variant1(iter) => iter.next(),
            ListIterator4::Variant2(iter) => iter.next(),
            ListIterator4::Variant3(iter) => iter.next(),
        }
    }
}
