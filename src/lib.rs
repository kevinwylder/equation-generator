#![allow(dead_code)]
use std::ptr;
use std::fmt;
use std::ops;

#[derive(Copy,Clone,Debug,PartialEq)]
enum Key {
    Digit(u8),
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal
}

impl Key {

    const OPERATORS: [Key; 5] = [Key::Plus, Key::Minus, Key::Multiply, Key::Divide, Key::Equal];

    fn parse(c: char) -> Result<Key, char> {
        match c {
            '+' => Ok(Key::Plus),
            '-' => Ok(Key::Minus),
            '*' => Ok(Key::Multiply),
            '/' => Ok(Key::Divide),
            '=' => Ok(Key::Equal),
            '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => 
                Ok(Key::Digit((
                    (c as i32) - ('0' as i32)) as u8
                )),
            _ => Err(c)
        }
    }

    // key_idx assigns a numeric value to the key between 0 and 14 (inclusive) for array indexing
    fn key_idx(self) -> usize {
        match self {
            Key::Digit(d) => {
                assert!(d < 10, "key index out of bounds");
                d as usize
            },
            Key::Plus => 10,
            Key::Minus => 11,
            Key::Multiply => 12,
            Key::Divide => 13,
            Key::Equal => 14,
        }
    }

    // integer_next increments the underlying integer if possible
    // returns true if the key was either rolled over, or is an operator
    fn integer_next(&mut self) -> bool {
        match *self {
            Key::Digit(d) => {
                assert!(d < 10, "key index out of bounds");
                if d != 9 {
                    *self = Key::Digit(d+1);
                    false
                } else {
                    *self = Key::Digit(0);
                    true
                }
            },
            _ => true,
        }
    }

    // operator_next increments the underlying operator to a different operator returns
    // true if the operation was successful and did not roll over back to the starting operator
    fn operator_next(&mut self) -> bool {
        match *self {
            Key::Plus => {
                *self = Key::Minus;
                true
            },
            Key::Minus => {
                *self = Key::Multiply;
                true
            }
            Key::Multiply => {
                *self = Key::Divide;
                true
            }
            Key::Divide => {
                *self = Key::Plus;
                false
            }
            _ => false
        }

    }

}

#[derive(Copy,Clone,PartialEq,Debug)]
enum Possibility {
    Certain,
    Impossible,
    Unknown,
}

struct PossibilityMatrix {
    // 15 keys * 10 slots
    matrix: [Possibility; 150],
}

impl ops::Index<(usize, Key)> for PossibilityMatrix {
    type Output = Possibility;

    fn index(&self, idx: (usize, Key)) -> &Possibility {
        let (slot, key) = idx;
        assert!(slot < 15, "slot index out of bounds");
        &self.matrix[key.key_idx() + 15 * slot]
    }
}

impl ops::IndexMut<(usize, Key)> for PossibilityMatrix {
    fn index_mut(&mut self, idx: (usize, Key)) -> &mut Possibility {
        let (slot, key) = idx;
        assert!(slot < 15, "slot index out of bounds");
        &mut self.matrix[key.key_idx() + 15 * slot]
    }
}

impl PossibilityMatrix {

    fn blank() -> Self {
        let mut matrix = Self{
            matrix: [Possibility::Unknown; 150],
        };

        // assign impossible to operators on the edges
        for slot in [0, 9] {
            for key in Key::OPERATORS {
                matrix[(slot, key)] = Possibility::Impossible;
            }
        }
        matrix[(0, Key::Digit(0))] = Possibility::Impossible;
        
        matrix
    }
    
    fn set_certain(&mut self, slot: ProgIndex, key: Key) {
        // sanity check
        assert_ne!(self[(slot, key)], Possibility::Impossible, "{:?} in slot {} was previously impossible", key, slot);

        let offset = slot * 15;
        for i in 0..15 {
            if i == key.key_idx() {
                self.matrix[offset + i] = Possibility::Certain
            } else {
                // If you are certain of a value, all other keys are impossible in that slot
                self.matrix[offset + i] = Possibility::Impossible
            }
        }
        // if the key was an operator, the surrounding slots aren't
        if Key::OPERATORS.contains(&key) {
            for operator in Key::OPERATORS {
                self.matrix[offset - 15 + operator.key_idx()] = Possibility::Impossible;
                self.matrix[offset + 15 + operator.key_idx()] = Possibility::Impossible;
            }
            // also the next digit can't be a 0 either
            self.matrix[offset + 15 + Key::Digit(0).key_idx()] = Possibility::Impossible
        }
    }

    fn set_impossible(&mut self, slot: ProgIndex, key: Key) {
        // sanity check
        assert_ne!(self[(slot, key)], Possibility::Certain, "{:?} in slot {} was previously certain", key, slot);

        self.matrix[slot * 15 + key.key_idx()] = Possibility::Impossible
    }
    
    fn int_bounds(&self, start: ProgIndex, width: IntWidth) -> (i32, i32) {
        
        (0, 0)
    }

}

struct Equation {
    keys: [Key; 10],
}

impl Equation {
    
}


#[derive(Debug)]
enum CompileError {
    UnexpectedOperator(Key),
    UnexpectedNumber(Key),
    LeadingZero,
    EndOfProgram,
    DuplicateEquals,
}

#[derive(PartialEq,Debug)]
enum EvalError {
    NotEquals,
    Fraction,
    DivZero,
}


enum AST {
    Equals(Box<AST>, Box<AST>),
    Upper(Box<AST>, UpperOp, Box<AST>),
    Lower(Box<AST>, LowerOp, Box<AST>),
    Integer(i32, ProgIndex, IntWidth),
}

// ProgIndex is the index into the program where this Key sequence starts
type ProgIndex = usize;
// IntWidth is the width of the integer in Key
type IntWidth = usize;

enum LowerOp {
    Mul,
    Div
}

enum UpperOp {
    Plus,
    Minus
}

trait KeyStream {
    fn next_key(&mut self) -> Option<Key>;
    fn peek_key(&mut self) -> Option<Key>;

    fn parse_int(&mut self) -> Result<(i32, IntWidth), CompileError> {
        let mut value = match self.next_key() {
            Some(Key::Digit(v)) => v as i32,
            Some(k) => return Err(CompileError::UnexpectedOperator(k)),
            None => return Err(CompileError::EndOfProgram)
        };
        let mut width = 1;
        if value == 0 {
            return Err(CompileError::LeadingZero)
        }
        loop {
            match self.peek_key() {
                Some(Key::Digit(v)) => {
                    self.next_key();
                    width += 1;
                    value = (value * 10) + (v as i32)
                }
                _ => return Ok((value, width))
            }
        }
    }
}

impl AST {

    pub fn compile<K: KeyStream>(data: &mut K) -> Result<AST, CompileError> {
        let (val, width) = data.parse_int()?;
        let mut tree = AST::Integer(val, 0, width);
        let mut index = width;
        loop {
            match data.next_key() {
                None => return Ok(tree),
                Some(k) => {
                    let (val, width) = data.parse_int()?;
                    let integer = AST::Integer(val, index + 1, width);
                    tree.append(k, integer)?;
                    index += 1 + width;
                }
            }
        }
    }

    // bounds computes the upper and lower bound for each expression
    fn bounds(self, constraints: &PossibilityMatrix) -> (i32, i32) {
        match self {
            AST::Equals(lhs, rhs) => {
                // "Equals" really computes the bounds of lhs and rhs differences
                let (lhs_low, lhs_high) = lhs.bounds(constraints);
                let (rhs_low, rhs_high) = rhs.bounds(constraints);
                (lhs_low - rhs_high, lhs_high - rhs_low)
            }
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => {
                        let (lhs_low, lhs_high) = lhs.bounds(constraints);
                        let (rhs_low, rhs_high) = rhs.bounds(constraints);
                        (lhs_low + rhs_low, lhs_high + rhs_high)
                    },
                    UpperOp::Minus => {
                        let (lhs_low, lhs_high) = lhs.bounds(constraints);
                        let (rhs_low, rhs_high) = rhs.bounds(constraints);
                        (lhs_low - rhs_high, lhs_high - rhs_low)
                    },
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => {
                        let (lhs_low, lhs_high) = lhs.bounds(constraints);
                        let (rhs_low, rhs_high) = rhs.bounds(constraints);
                        (lhs_low * rhs_low, lhs_high * rhs_high)
                    },
                    LowerOp::Div => {
                        let (lhs_low, lhs_high) = lhs.bounds(constraints);
                        let (rhs_low, rhs_high) = rhs.bounds(constraints);
                        (lhs_low / rhs_high, lhs_high / rhs_low)
                    },
                }
            }
            AST::Integer(_, idx, width) => constraints.int_bounds(idx, width)
        }
    }

    
    // https://stackoverflow.com/questions/29570781/temporarily-move-out-of-borrowed-content
    // https://github.com/alecmocatta/replace_with/blob/master/src/lib.rs
    fn unsafe_replace_with<F: FnOnce(Self) -> Self>(&mut self, construct: F) {
        unsafe {
            let old_self = ptr::read(self);
            // NOTE: some terrible things may happen if you panic here
            let new_self = construct(old_self);
            ptr::write(self, new_self);
        }
    }

    fn append(&mut self, k: Key, integer: AST) -> Result<(), CompileError> {
        match k {
            Key::Digit(_) => return Err(CompileError::UnexpectedNumber(k)),
            Key::Multiply => self.append_lower(LowerOp::Mul, integer),
            Key::Divide => self.append_lower(LowerOp::Div, integer),
            Key::Plus =>  self.append_upper(UpperOp::Plus, integer),
            Key::Minus => self.append_upper(UpperOp::Minus, integer),
            Key::Equal => self.append_equals(integer)?,
        }
        Ok(())
    }

    fn append_equals(&mut self, integer: AST) -> Result<(), CompileError> {
        match self {
            AST::Equals(_, _) => Err(CompileError::DuplicateEquals),
            _ => {
                self.unsafe_replace_with(
                    |_self| AST::Equals(
                        Box::new(_self),
                        Box::new(integer),
                    )
                );
                Ok(())
            }
        }
    }

    fn append_upper(&mut self, op: UpperOp, integer: AST) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => rhs,
            _ => self
        };
        side.unsafe_replace_with(
            |_self| AST::Upper(
                Box::new(_self),
                op,
                Box::new(integer),
            )
        );
    }

    fn append_lower(&mut self, op: LowerOp, integer: AST) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => return rhs.append_lower(op, integer),
            AST::Upper(_, _, rhs) => rhs,
            _ => self,
        };
        side.unsafe_replace_with(
            |_self| AST::Lower(
                Box::new(_self),
                op,
                Box::new(integer)
            )
        )
    }

    fn value(&self) -> Result<i32, EvalError> {
        Ok(
            match self {
                AST::Integer(i, _, _) => *i,
                AST::Lower(lhs, op, rhs) =>
                    match op {
                        LowerOp::Mul => lhs.value()? * rhs.value()?,
                        LowerOp::Div => {
                            let rhs_value = rhs.value()?;
                            if rhs_value == 0 {
                                return Err(EvalError::DivZero)
                            }
                            let lhs_value = lhs.value()?;
                            if lhs_value % rhs_value != 0 {
                                return Err(EvalError::Fraction)
                            }
                            lhs_value / rhs_value
                        }
                    }
                AST::Upper(lhs, op, rhs) => {
                    match op {
                        UpperOp::Plus => lhs.value()? + rhs.value()?,
                        UpperOp::Minus => lhs.value()? - rhs.value()?
                    }
                }
                AST::Equals(lhs, rhs) => {
                    if lhs.value()? != rhs.value()? {
                        return Err(EvalError::NotEquals)
                    }
                    0
                }
            }
        )
    }

}

impl fmt::Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AST::Equals(lhs, rhs) => write!(f, "{:?}={:?}", lhs, rhs),
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => write!(f, "({:?}+{:?})", lhs, rhs),
                    UpperOp::Minus => write!(f, "({:?}-{:?})", lhs, rhs),
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => write!(f, "({:?}*{:?})", lhs, rhs),
                    LowerOp::Div => write!(f, "({:?}/{:?})", lhs, rhs)
                }
            },
            AST::Integer(i, _, _) => i.fmt(f)
        }
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AST::Equals(lhs, rhs) => write!(f, "{}={}", lhs, rhs),
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => write!(f, "{}+{}", lhs, rhs),
                    UpperOp::Minus => write!(f, "{}-{}", lhs, rhs),
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => write!(f, "{}*{}", lhs, rhs),
                    LowerOp::Div => write!(f, "{}/{}", lhs, rhs)
                }
            },
            AST::Integer(i, _, _) => i.fmt(f)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;
    use std::iter;

    impl KeyStream for iter::Peekable<str::Chars<'static>> {

        fn next_key(&mut self) -> Option<Key> {
            if let Some(c) = self.next() {
                match Key::parse(c) {
                    Ok(k) => Some(k),
                    Err(_) => panic!("cannot parse {}", c),
                }
            } else {
                None
            }
        }

        fn peek_key(&mut self) -> Option<Key> {
            if let Some(c) = self.peek() {
                match Key::parse(*c) {
                    Ok(k) => Some(k),
                    Err(_) => panic!("cannot parse {}", c),
                }
            } else {
                None
            }
        }
    }

    #[test]
    fn test_compile_programs() {
        let tests = vec![
            ("1", "1", Ok(1)),
            ("10", "10", Ok(10)),
            ("2+2", "(2+2)", Ok(4)),
            ("13+2", "(13+2)", Ok(15)),
            ("2*3+4", "((2*3)+4)", Ok(10)),
            ("2+3*4", "(2+(3*4))", Ok(14)),
            ("2+3*4=84", "(2+(3*4))=84", Err(EvalError::NotEquals)),
            ("2+3*4=84-70", "(2+(3*4))=(84-70)", Ok(0)),
        ];

        for (i, test) in tests.iter().enumerate() {
            let (prog, repr, eval) = test;
            let mut stream = prog.chars().peekable();
            let ast = match AST::compile(&mut stream) {
                Err(e) => panic!("program {} `{}` failed to compile - {:?}", i, prog, e),
                Ok(ast) => ast,
            };
            
            assert_eq!(prog.to_string(), format!("{}", ast));
            assert_eq!(repr.to_string(), format!("{:?}", ast));
            assert_eq!(ast.value(), *eval);
        }
    }

    #[test]
    fn test_invalid_programs() {
        let progs = vec![
            "0000",
            "+1",
            "1+",
            "1=+",
            "-1",
            "01",
            "=",
        ];
        for prog in progs {
            let mut stream = prog.chars().peekable();
            if let Ok(_) = AST::compile(&mut stream) {
                panic!("{} should not compile", prog)
            }
        }
    }

    #[test]
    fn test_constraint_matrix() {
        let mut matrix = PossibilityMatrix::blank();
        assert_eq!(matrix[(0, Key::Equal)], Possibility::Impossible);
        assert_eq!(matrix[(0, Key::Digit(0))], Possibility::Impossible);

        matrix.set_certain(1, Key::Multiply);
        assert_eq!(matrix[(2, Key::Divide)], Possibility::Impossible);
        assert_eq!(matrix[(2, Key::Digit(1))], Possibility::Unknown);
        assert_eq!(matrix[(2, Key::Digit(0))], Possibility::Impossible);

        matrix.set_impossible(2, Key::Digit(1));
        assert_eq!(matrix[(2, Key::Digit(1))], Possibility::Impossible);
        assert_eq!(matrix[(2, Key::Digit(2))], Possibility::Unknown);
    }

}

