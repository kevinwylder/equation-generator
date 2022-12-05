use std::fmt;

struct Constant {
    val: u32,
    min: u32,
    max: u32,
}

impl Constant {
    fn of_size(len: u8) -> Self {
        assert!(len > 0, "cannot create 0 len constant");
        let min = iexp10(len);
        let max = iexp10(len + 1);
        Constant{
            val: min,
            min,
            max,
        }
    }

}

fn iexp10(v: u8) -> u32 {
    (10 as u32).pow((v - 1) as u32)
}

fn ilog10(v: i32) -> u8 {
    (v as f32).log10() as u8
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


trait ExpressionGenerator {
    fn upper_bound(&self) -> i32;
    fn lower_bound(&self) -> i32;
    fn value(&self) -> i32;

    // set_value tries to make the expression equal to the given value. if the value is impossible to
    // be that value, it returns false
    //
    // when set_value returns false, it may have changed value.
    //
    // calling set_value puts the ExpressionGenerator in the "first" state, and calling `next` will
    // yield new values until all have been found, at which point `next` returns false
    fn set_value(&mut self, value: i32) -> bool;

    // next moves to the expression without changing its current value
    // returns true if it has a new value, and false if it rolled over back to the first position
    fn next(&mut self) -> bool;

    // next_value changes the value of the expression by incrementing the underlying constants. It
    // returns true if a new value was taken without rolling back over to the starting constants
    fn next_value(&mut self) -> bool;
}


impl ExpressionGenerator for Constant {

    fn upper_bound(&self) -> i32 {
        (self.max - 1) as i32
    }

    fn lower_bound(&self) -> i32 {
        self.min as i32
    }

    fn set_value(&mut self, val: i32) -> bool {
        if val < 1 {
            return false
        }
        let uval = val as u32;
        if uval >= self.min && uval < self.max {
            self.val = uval;
            true
        } else {
            false
        }
    }

    fn next(&mut self) -> bool {
       false
    }

    fn value(&self) -> i32 {
        self.val as i32
    }

    fn next_value(&mut self) -> bool {
        self.val += 1;
        if self.val == self.max {
            self.val = self.min;
            false
        } else {
            true
        }
    }

}


impl O1 {

    fn mul_iter_constant_up(o1: &mut O1, c: &mut Constant, val: i32) -> bool {
        if val < c.value() {
            return false
        }
        for denominator in c.value()..(c.upper_bound().min(val) + 1) {
            if val % denominator == 0 && o1.set_value(val / denominator) && c.set_value(denominator) {
                return true
            }
        }
        false
    }

    fn div_iter_constant_up(o1: &mut O1, c: &mut Constant, val: i32) -> bool {
        let upper_mul = o1.upper_bound() / val;
        if upper_mul < c.value() {
            return false
        }
        for v in c.value()..(c.upper_bound().min(upper_mul) + 1) {
            if o1.set_value(val * v) && c.set_value(v) {
                return true
            }
        }
        false
    }

    fn div_can_set_denominator(denominator: usize, next_divisor: i32, o1: &mut O1) -> bool {
        let next_divisors = next_divisor..(o1.upper_bound() + 1);
        for divisor in next_divisors.step_by(denominator) {
            if o1.set_value(divisor) {
                return true
            }
        }
        false
    }

}

impl ExpressionGenerator for O1 {

    fn upper_bound(&self) -> i32 {
        match self {
            O1::Constant(c) => c.upper_bound(),
            O1::Multiplication(o, c) => o.upper_bound() * c.upper_bound(),
            O1::Division(o, c) => o.upper_bound() / c.lower_bound(),
        }
    }

    fn lower_bound(&self) -> i32 {
        match self {
            O1::Constant(c) => c.lower_bound(),
            O1::Multiplication(o, c) => o.lower_bound() * c.lower_bound(),
            O1::Division(o, c) => o.lower_bound() / c.upper_bound(),
        }
    }

    fn value(&self) -> i32 {
        match self {
            O1::Constant(c) => c.value(),
            O1::Multiplication(o, c) => o.value() * c.value(),
            O1::Division(o, c) => o.value() / c.value()
        }
    }

    fn set_value(&mut self, val: i32) -> bool {
        match self {
            O1::Constant(c) => {
                c.set_value(val)
            },
            O1::Multiplication(a, c) => {
                c.val = c.min;
                Self::mul_iter_constant_up(a, c, val)
            },
            O1::Division(a, c) => {
                c.val = c.min;
                Self::div_iter_constant_up(a, c, val)
            }
        }
    }

    fn next(&mut self) -> bool {
        let v = self.value();
        match self {
            O1::Constant(_) => false,
            O1::Multiplication(o1, c) => { 
                if o1.next() {
                    return true
                }
                c.val = c.val + 1;
                if Self::mul_iter_constant_up(o1, c, v) {
                    true
                } else {
                    self.set_value(v);
                    false
                }
            },
            O1::Division(o1, c) => {
                if o1.next() {
                    return true
                }
                c.val = c.val + 1;
                if Self::div_iter_constant_up(o1, c, v) {
                    true
                } else {
                    self.set_value(v);
                    false
                }
            }
        }
    }

    fn next_value(&mut self) -> bool {
        match self {
            O1::Constant(c) => c.next_value(),
            O1::Multiplication(o1, c) => {
                if c.next_value() {
                    return true
                }
                o1.next_value()
            },
            O1::Division(o1, c) => {
                if o1.next() {
                    return true
                }
                let next_divisor = ((o1.value() / c.value()) + 1) * c.value();
                if Self::div_can_set_denominator(c.value() as usize, next_divisor, o1) {
                    return true
                }
                let mut did_roll_over = false;
                loop {
                    let just_rolled_over = !c.next_value();
                    if did_roll_over && just_rolled_over {
                        panic!("I just rolled over twice while trying to find the next possible divisor! You may have constructed an impossible to solve equation {} / {}", o1, c);
                    }
                    did_roll_over |= just_rolled_over;
                    if Self::div_can_set_denominator(c.value() as usize, c.value(), o1) {
                        return did_roll_over
                    }
                }
            }
        }
    }
}

impl O2 {

    fn add_iter_o1_up(o2: &mut O2, o1: &mut O1, val: i32, offset: i32) -> bool {
        let o1_high = o1.upper_bound();
        let o2_high = o2.upper_bound();

        if val < o1.lower_bound() + o2.lower_bound() || val > o1_high + o2_high {
            return false
        }

        let mut curr = o1.value() + offset;
        while curr <= o1_high && val - curr <= o2_high {
            if o1.set_value(curr) && o2.set_value(val - curr) {
                return true
            }
            curr += 1;
        }
        false
    }

    fn sub_iter_o1_up(o2: &mut O2, o1: &mut O1, val: i32, offset: i32) -> bool {
        let o1_high = o1.upper_bound();
        let o2_high = o2.upper_bound();

        if val < o2.lower_bound() - o1_high || val > o2_high - o1.lower_bound() {
            return false
        }

        let mut curr = o1.value() + offset;
        while curr <= o1_high && val + curr <= o2_high {
            if o1.set_value(curr) && o2.set_value(val + curr) {
                return true
            }
            curr += 1;
        }
        false
    }

}

impl ExpressionGenerator for O2 {

    fn upper_bound(&self) -> i32 {
        match self {
            O2::O1(o1) => o1.upper_bound(),
            O2::Addition(o2, o1) => o2.upper_bound() + o1.upper_bound(),
            O2::Subtraction(o2, o1) => o2.upper_bound() - o1.lower_bound(),
        }
    }

    fn lower_bound(&self) -> i32 {
        match self {
            O2::O1(o1) => o1.upper_bound(),
            O2::Addition(o2, o1) => o2.lower_bound() + o1.lower_bound(),
            O2::Subtraction(o2, o1) => o2.lower_bound() - o1.upper_bound(),
        }
    }

    fn value(&self) -> i32 {
        match self {
            O2::O1(c) => c.value(),
            O2::Addition(a, b) => a.value() + b.value(),
            O2::Subtraction(a, b) => a.value() - b.value(),
        }
    }

    fn set_value(&mut self, val: i32) -> bool {
        match self {
            O2::O1(o1) => o1.set_value(val),
            O2::Addition(o2, o1) => {
                assert!(o1.set_value(o1.lower_bound()), "cannot set o1 {} to the lower bound!", o1);
                Self::add_iter_o1_up(o2, o1, val, 0)
            },
            O2::Subtraction(o2, o1) => {
                assert!(o1.set_value(o1.lower_bound()), "cannot set o1 {} to the lower bound!", o1);
                Self::sub_iter_o1_up(o2, o1, val, 0)
            }
        }
    }

    fn next(&mut self) -> bool {
        let val = self.value();
        match self {
            O2::O1(c) => c.next(),
            O2::Addition(o2, o1) => {
                if o2.next() {
                    return true
                }
                if o1.next() {
                    return true
                }
                Self::add_iter_o1_up(o2, o1, val, 1)
                
            },
            O2::Subtraction(o2, o1) => {
                if o2.next() {
                    return true
                }
                if o1.next() {
                    return true
                }
                Self::sub_iter_o1_up(o2, o1, val, 1)
            },
        }
    }

    fn next_value(&mut self) -> bool {
        match self {
            O2::O1(o1) => o1.next_value(),
            O2::Addition(o2, o1) | O2::Subtraction(o2, o1) => {
                if o1.next_value() {
                    return true
                }
                o2.next_value()
            },
        }
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
        if self.length <= 2 {
            // we can't generate anything other than a constant for O1 lengths less than 2
            return false
        }

        if let Some(ref mut lhs) = self.lhs {
            // increment lhs see if we need to do anything 
            if lhs.next() {
                return true
            }
            // lhs rolled over!

            // make lhs shorter?
            let min_lhs = if self.is_mul {
                1
            } else {
                ilog10(lhs.value().upper_bound()) + 1
            };
            if lhs.length > min_lhs {
                self.lhs = Some(Box::new(O1Generator::of_length(lhs.length - 1)));
                return true
            }
            // lhs can't be any shorter! 

            // switch from mul to div?
            self.is_mul = !self.is_mul;
            if !self.is_mul {
                self.lhs = Some(Box::new(O1Generator::of_length(self.length - 2)));
                return true;
            }
            // we already did div!

            // really rolled over this time
            self.lhs = None;
            false
        } else {
            self.lhs = Some(Box::new(O1Generator::of_length(self.length - 2)));
            true
        }
    }

}


struct O2Generator {
    len: u8,
    is_addition: bool,
    lhs: Option<Box<O2Generator>>
}

impl O2Generator {

    fn of_length(len: u8) -> Self {
        Self{
            len,
            is_addition: true,
            lhs: None
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn o1_mul_set_value() {
        let mut one_times_one = O1::Multiplication(
            Box::new(O1::Constant(Constant::of_size(1))), 
            Constant::of_size(1)
        );
        assert!(one_times_one.set_value(9), "can set X*Y=9");
        assert_eq!(format!("{}", one_times_one), "9*1");
        assert!(one_times_one.next());
        assert_eq!(format!("{}", one_times_one), "3*3");
        assert!(one_times_one.next());
        assert_eq!(format!("{}", one_times_one), "1*9");
        assert!(!one_times_one.next());
        assert_eq!(format!("{}", one_times_one), "9*1");

        assert!(one_times_one.set_value(25), "can set X*Y=25");
        assert_eq!(format!("{}", one_times_one), "5*5");
        assert!(!one_times_one.next());
        assert_eq!(format!("{}", one_times_one), "5*5");
    }

    #[test]
    fn o1_div_set_value() {
        let mut two_divide_one = O1::Division(
            Box::new(O1::Constant(Constant::of_size(2))), 
            Constant::of_size(1)
        );
        assert!(two_divide_one.set_value(9), "can set XX/Y=9");
        assert_eq!(format!("{}", two_divide_one), "18/2");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "27/3");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "36/4");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "45/5");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "54/6");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "63/7");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "72/8");
        assert!(two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "81/9");
        assert!(!two_divide_one.next());
        assert_eq!(format!("{}", two_divide_one), "18/2");

    }

    #[test]
    fn o2_add_set_value() {
        let mut one_plus_one_times_one = O2::Addition(
            Box::new(O2::O1(O1::Constant(Constant::of_size(1)))),
            O1::Multiplication(
                Box::new(O1::Constant(Constant::of_size(1))),
                Constant::of_size(1)
            )
        );
        assert!(one_plus_one_times_one.set_value(10), "set X+(Y*Z)=10");
        let answers = vec![
            "9+1*1", "8+2*1", "8+1*2", "7+3*1", "7+1*3", "6+4*1", "6+2*2", "6+1*4", "5+5*1", "5+1*5",
            "4+6*1", "4+3*2", "4+2*3", "4+1*6", "3+7*1", "3+1*7", "2+8*1", "2+4*2", "2+2*4", "2+1*8",
            "1+9*1", "1+3*3",
        ];
        for (i, answer) in answers.iter().enumerate() {
            println!("{} - {}", i, answer);
            assert_eq!(format!("{}", one_plus_one_times_one), *answer);
            assert!(one_plus_one_times_one.next());
        } 
        assert_eq!(format!("{}", one_plus_one_times_one), "1+1*9");
        assert!(!one_plus_one_times_one.next(), "{}", one_plus_one_times_one);
    }

    #[test]
    fn o1_div_next_values() {
        let mut one_times_one_div_two = O1::Division(
            Box::new(O1::Multiplication(
                Box::new(O1::Constant(
                    Constant::of_size(1)
                )),
                Constant::of_size(1)
            )), 
            Constant::of_size(2)
        );
        // NOTE it is the creator's responsibility to seed division with a valid state 
        // here we have the default 1*1/10 which is *not* a valid state 
        // calling `next_value` should iterate it forward to the first valid state if one exists
        let answers = vec![
            "5*2/10", "2*5/10", "5*4/10", "4*5/10", "6*5/10", "5*6/10", "8*5/10", "5*8/10", "6*2/12",
            "4*3/12", "3*4/12", "2*6/12", "8*3/12", "6*4/12", "4*6/12", "3*8/12", "9*4/12", "6*6/12",
            "4*9/12", "8*6/12", "6*8/12", "9*8/12", "8*9/12", "7*2/14", "2*7/14", "7*4/14", "4*7/14",
        ];
        for answer in &answers {
            one_times_one_div_two.next_value();
            assert_eq!(format!("{}", one_times_one_div_two), *answer);
        }
    }

    #[test]
    fn test_o1_generator() {
        let mut generator = O1Generator::of_length(8);
        let answers = vec![
            "10000000", "100000*1", "1000*1*1", "10*1*1*1", "1*10*1*1", "10/1*1*1", "100*10*1", 
            "1*1*10*1", "1/1*10*1", "10*100*1", "1*1000*1", "1000/1*1", "10*1/1*1", "1*10/1*1",
            "10/1/1*1", "10000*10", "100*1*10", "1*1*1*10", "1/1*1*10", "10*10*10", "1*100*10",
            "100/1*10", "1*1/1*10", "1/1/1*10", "1000*100", "10*1*100", "1*10*100", "10/1*100",
            "100*1000", "1*1*1000", "1/1*1000", "10*10000", "1*100000", "100000/1", "1000*1/1",
            "10*1*1/1", "1*10*1/1", "10/1*1/1", "100*10/1", "1*1*10/1", "1/1*10/1", "10*100/1",
            "1*1000/1", "1000/1/1", "10*1/1/1", "1*10/1/1", 
        ];
        for answer in &answers {
            let mut eqn = generator.value();
            println!("{}", eqn);
            assert_eq!(format!("{}", eqn), *answer);
            assert!(eqn.next_value());
            assert!(generator.next());
        }
        assert_eq!(format!("{}", generator.value()), "10/1/1/1");

    }
}

fn main() {
}
