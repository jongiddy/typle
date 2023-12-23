use typle::typle;
struct MyStruct<T> {
    pub t: T,
}
impl MyStruct<(u32,)> {
    fn max(&self) -> Option<u32> {
        #[allow(unused_mut)]
        let mut max = self.t.0;
        { () }
        Some(max)
    }
}
impl MyStruct<(u32, u32)> {
    fn max(&self) -> Option<u32> {
        #[allow(unused_mut)]
        let mut max = self.t.0;
        {
            {
                if self.t.1 > max {
                    max = self.t.1;
                }
            }
            ()
        }
        Some(max)
    }
}
impl MyStruct<(u32, u32, u32)> {
    fn max(&self) -> Option<u32> {
        #[allow(unused_mut)]
        let mut max = self.t.0;
        {
            {
                if self.t.1 > max {
                    max = self.t.1;
                }
            }
            {
                if self.t.2 > max {
                    max = self.t.2;
                }
            }
            ()
        }
        Some(max)
    }
}
