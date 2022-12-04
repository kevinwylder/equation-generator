use std::fmt::{self, Display};

struct Constant {
    val: u32,
    min: u32,
    max: u32,
}

impl Constant {
    fn of_size(len: u8) -> Self {
        let base: u32 = 10;
        let max = base.pow(len as u32);
        let min = if len == 0 {
            max
        } else {
            base.pow((len - 1) as u32)
        };
        Constant{
            val: min,
            min,
            max,
        }
    }
}

// O1 is the first operator to evaluate
enum O1 {
    Constant(Constant),
    Multiplication(Box<O1>, Constant),
    Division(Box<O1>, Constant)
}

// O2 is the second operator to evaluate
enum O2 {
    O1(O1),
    Addition(Box<O2>, O1),
    Subtraction(Box<O2>, O1)
}

struct Equation {
    lhs: O2,
    rhs: O2
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




trait Generator<V> {
    fn value(&self) -> V;
    // next moves to the next position. 
    // It returns true if it rolled over back to the first position
    fn next(&mut self) -> bool;
}


impl Generator<u32> for Constant {

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

fn pair_next<V, L: Generator<V>, R: Generator<V>>(a: &mut L, b: &mut R) -> bool {
    if a.next() {
        if b.next() {
            return true;
        }
    }
    false
}

fn pair_next_until<V, L: Generator<V> + Display, R: Generator<V> + Display>(
    a: &mut L,
    b: &mut R,
    is_bad: fn(a: &mut L, b: &mut R) -> bool
) -> bool {
    let mut did_roll_over = pair_next::<V, L, R>(a, b);
    while is_bad(a, b) {
        if pair_next::<V, L, R>(a, b) {
            if did_roll_over {
                break
            }
            did_roll_over = true
        }
    }
    did_roll_over
}

impl Generator<u32> for O1 {

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
            O1::Multiplication(a, b) => pair_next::<u32, O1, Constant>(a, b),
            O1::Division(a, b) => pair_next_until::<u32, O1, Constant>(a, b, |a, b| a.value() % b.value() != 0),
        }
    }
}

impl Generator<u32> for O2 {

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
            O2::Addition(a, b) => pair_next::<u32, O2, O1>(a, b),
            O2::Subtraction(a, b) => pair_next::<u32, O2, O1>(a, b),
        }
    }
}

impl Generator<u32> for Equation {

    fn value(&self) -> u32 {
        self.lhs.value()
    }

    fn next(&mut self) -> bool {
        pair_next_until(&mut self.lhs, &mut self.rhs, |a, b| a.value() != b.value())
    }
}

struct O1Generator {
    length: u8,
    is_mul: bool,
    lhs: Option<Box<O1Generator>>
}

impl O1Generator {
    
    fn of_length(length: u8) -> Self {
        Self{
            length,
            is_mul: true,
            lhs: None,
        }
    }
}

impl Generator<Box<O1>> for O1Generator {

    fn value(&self) -> Box<O1> {
        Box::new(
            match &self.lhs {
                None => O1::Constant(Constant::of_size(self.length)),
                Some(gen) => {
                    let value = gen.value();
                    let const_width = self.length - gen.length - 1;
                    let constant = Constant::of_size(const_width);
                    if self.is_mul {
                        O1::Multiplication(value, constant)
                    } else {
                        O1::Division(value, constant)
                    }
                }
            }
        )
    }

    fn next(&mut self) -> bool {
        if self.length < 2 {
            // we can't generate anything other than a constant for O1 lengths less than 2
            return true;
        }

        if let Some(ref mut lhs) = self.lhs {
            // increment lhs see if we need to do anything 
            if !lhs.next() {
                return false;
            }
            // lhs rolled over!

            // switch from mul to div?
            self.is_mul = !self.is_mul;
            if !self.is_mul {
                return false;
            }
            // we already did div!

            // make lhs shorter?
            if lhs.length >= 2 {
                self.lhs = Some(Box::new(O1Generator::of_length(lhs.length - 1)));
                return false;
            }
            // lhs can't be any shorter! 

            // really rolled over this time
            self.lhs = None;
            true
        } else {
            self.lhs = Some(Box::new(O1Generator::of_length(self.length - 2)));
            false
        }
    }

}


fn main() {
    let mut generator = O1Generator::of_length(3);
    loop {
        // TODO initialization for equation so it isn't invalid at the start
        let mut a = Equation{
            lhs: O2::O1(*generator.value()),
            rhs: O2::O1(O1::Constant(Constant::of_size(1))),
        };
        println!("{}", a);

        if generator.next() {
            break;
        }

        while !a.next() {
            println!("{}", a);
        }
    }
}
