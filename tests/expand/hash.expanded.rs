use typle::typle;
impl Hash for () {
    #[inline]
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}
impl<T0> Hash for (T0,)
where
    T0: Hash,
    T0: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1> Hash for (T0, T1)
where
    T0: Hash,
    T1: Hash,
    T1: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2> Hash for (T0, T1, T2)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T2: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3> Hash for (T0, T1, T2, T3)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T3: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4> Hash for (T0, T1, T2, T3, T4)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T4: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5> Hash for (T0, T1, T2, T3, T4, T5)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T5: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6> Hash for (T0, T1, T2, T3, T4, T5, T6)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T6: Hash,
    T6: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            {
                self.6.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7> Hash for (T0, T1, T2, T3, T4, T5, T6, T7)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T6: Hash,
    T7: Hash,
    T7: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            {
                self.6.hash(state);
            }
            {
                self.7.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8> Hash for (T0, T1, T2, T3, T4, T5, T6, T7, T8)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T6: Hash,
    T7: Hash,
    T8: Hash,
    T8: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            {
                self.6.hash(state);
            }
            {
                self.7.hash(state);
            }
            {
                self.8.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9> Hash
for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T6: Hash,
    T7: Hash,
    T8: Hash,
    T9: Hash,
    T9: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            {
                self.6.hash(state);
            }
            {
                self.7.hash(state);
            }
            {
                self.8.hash(state);
            }
            {
                self.9.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> Hash
for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T6: Hash,
    T7: Hash,
    T8: Hash,
    T9: Hash,
    T10: Hash,
    T10: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            {
                self.6.hash(state);
            }
            {
                self.7.hash(state);
            }
            {
                self.8.hash(state);
            }
            {
                self.9.hash(state);
            }
            {
                self.10.hash(state);
            }
            ()
        }
    }
}
impl<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> Hash
for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
where
    T0: Hash,
    T1: Hash,
    T2: Hash,
    T3: Hash,
    T4: Hash,
    T5: Hash,
    T6: Hash,
    T7: Hash,
    T8: Hash,
    T9: Hash,
    T10: Hash,
    T11: Hash,
    T11: ?Sized,
{
    #[inline]
    fn hash<S: Hasher>(&self, state: &mut S) {
        {
            {
                self.0.hash(state);
            }
            {
                self.1.hash(state);
            }
            {
                self.2.hash(state);
            }
            {
                self.3.hash(state);
            }
            {
                self.4.hash(state);
            }
            {
                self.5.hash(state);
            }
            {
                self.6.hash(state);
            }
            {
                self.7.hash(state);
            }
            {
                self.8.hash(state);
            }
            {
                self.9.hash(state);
            }
            {
                self.10.hash(state);
            }
            {
                self.11.hash(state);
            }
            ()
        }
    }
}
