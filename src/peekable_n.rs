use std::fmt::Debug;

pub struct PeekableN<I, const C: usize>
where
    I: Iterator,
{
    iter: I,
    buf: [Option<I::Item>; C],
}

impl<I, const C: usize> Debug for PeekableN<I, C>
where
    I: Iterator,
    I::Item: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PeekableN")
            .field("iter", &"iter")
            .field("buf", &self.buf)
            .finish()
    }
}

impl<I, const C: usize> Iterator for PeekableN<I, C>
where
    I: Iterator,
    I::Item: Debug,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut index = C - 1;
        loop {
            if let Some(v) = self.buf[index].take() {
                break Some(v);
            }
            if index == 0 {
                break None;
            }
            index -= 1;
        }
        .or_else(|| self.iter.next())
    }
}

impl<I, const C: usize> PeekableN<I, C>
where
    I: Iterator,
    I::Item: Debug,
{
    pub fn peek(&mut self) -> Option<&I::Item> {
        self.peekn::<0>()
    }

    pub fn peekn<const N: usize>(&mut self) -> Option<&I::Item> {
        assert!(N < C);

        if self.buf[N].is_some() {
            return self.buf[N].as_ref();
        }

        for i in 0..=N {
            if self.buf[i].is_none() {
                let val = self.iter.next();
                #[allow(clippy::question_mark)]
                if val.is_none() {
                    return None;
                }
                self.buf[i] = val;
            }
        }

        self.buf[N].as_ref()
    }
}

pub trait NPeekable {
    fn peekable_n<const C: usize>(self) -> PeekableN<Self, C>
    where
        Self: Iterator + Sized,
    {
        PeekableN {
            iter: self,
            buf: std::array::from_fn(|_| None),
        }
    }
}

impl<T> NPeekable for T where T: Iterator {}
