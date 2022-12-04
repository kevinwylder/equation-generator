use std::fmt::{self, Display};

struct Constant {
    len: u8,
    val: u32,
    min: u32,
    max: u32,
}

impl Constant {
    fn of_size(len: u8) -> O1 {
        assert!(len != 0, "you can't have a constant of len 0, dingus");
        let base: u32 = 10;
        let min = base.pow((len - 1) as u32);
        let max = base.pow(len as u32);
        O1::Constant(Constant{
            len,
            val: min,
            min,
            max,
        })
    }
}

enum O1 {
    Constant(Constant),
    Multiplication(Box<O1>, Constant),
    Division(Box<O1>, Constant)
}

enum O2 {
    O1(O1),
    Addition(Box<O2>, O1),
    Subtraction(Box<O2>, O1)
}

struct Equation {
    lhs: O2,
    rhs: O2
}


trait Generator {
    fn len(&self) -> u8;
    fn value(&self) -> u32;
    // next moves to the next position. 
    // It returns true if it rolled over back to the first position
    fn next(&mut self) -> bool;
}


impl Generator for Constant {
    fn len(&self) -> u8 {
        self.len
    }

    fn value(&self) -> u32 {
        self.val
    }

    fn next(&mut self) -> bool {
        self.val = self.val + 1;
        if self.val == self.max {
            self.val = self.min;
            true
        } else {
            false
        }
    }
}

fn pair_next<L: Generator, R: Generator>(a: &mut L, b: &mut R) -> bool {
    if a.next() {
        if b.next() {
            return true;
        }
    }
    false
}

fn pair_next_until<L: Generator + Display, R: Generator + Display>(a: &mut L, b: &mut R, dbg: char, is_bad: fn(a: &mut L, b: &mut R) -> bool) -> bool {
        let mut did_roll_over = pair_next::<L, R>(a, b);
        while is_bad(a, b) {
            if pair_next::<L, R>(a, b) {
                if did_roll_over {
                    panic!("rolled over twice while incrementing equation! why? {} {} {}", a, dbg, b);
                }
                did_roll_over = true
            }
        }
        did_roll_over
}

impl Generator for O1 {
    fn len(&self) -> u8 {
        match self {
            O1::Constant(c) => c.len(),
            O1::Multiplication(a, b) => a.len() + b.len() + 1,
            O1::Division(a, b) => a.len() + b.len() + 1,
        }
    }

    fn value(&self) -> u32 {
        match self {
            O1::Constant(c) => c.value(),
            O1::Multiplication(a, b) => a.value() * b.value(),
            O1::Division(a, b) => a.value() / b.value(),
        }
    }

    fn next(&mut self) -> bool {
        match self {
            O1::Constant(c) => c.next(),
            O1::Multiplication(a, b) => pair_next::<O1, Constant>(a, b),
            O1::Division(a, b) => pair_next_until::<O1, Constant>(a, b, '/', |a, b| a.value() % b.value() != 0),
        }
    }
}

impl Generator for O2 {
    fn len(&self) -> u8 {
        match self {
            O2::O1(c) => c.len(),
            O2::Addition(a, b) => a.len() + b.len() + 1,
            O2::Subtraction(a, b) => a.len() + b.len() + 1,
        }
    }

    fn value(&self) -> u32 {
        match self {
            O2::O1(c) => c.value(),
            O2::Addition(a, b) => a.value() + b.value(),
            O2::Subtraction(a, b) => a.value() - b.value(),
        }
    }

    fn next(&mut self) -> bool {
        match self {
            O2::O1(c) => c.next(),
            O2::Addition(a, b) => pair_next::<O2, O1>(a, b),
            O2::Subtraction(a, b) => pair_next::<O2, O1>(a, b),
        }
    }
}

impl Generator for Equation {
    fn len(&self) -> u8 {
        self.lhs.len() + self.rhs.len() + 1
    }

    fn value(&self) -> u32 {
        self.lhs.value()
    }

    fn next(&mut self) -> bool {
        pair_next_until(&mut self.lhs, &mut self.rhs, '=', |a, b| a.value() != b.value())
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.val.fmt(f)
    }
}

impl fmt::Display for O1 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            O1::Constant(c) => c.fmt(f),
            O1::Multiplication(a, b) => write!(f, "{}*{}", a, b),
            O1::Division(a, b) => write!(f, "{}/{}", a, b),
        }
    }
}

impl fmt::Display for O2 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            O2::O1(c) => c.fmt(f),
            O2::Addition(a, b) => write!(f, "{}+{}", a, b),
            O2::Subtraction(a, b) => write!(f, "{}-{}", a, b),
        }
    }
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}={}", self.lhs, self.rhs)
    }
}

fn main() {
    let mut a = Equation{
        lhs: O2::Addition(Box::new(O2::O1(Constant::of_size(1))), Constant::of_size(1)),
        rhs: O2::O1(Constant::of_size(1)),
    };
    while !a.next() {
        println!("{}", a);
    }
}
