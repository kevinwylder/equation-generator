use nerdle::PossibilityMatrix;

fn main() {
    let mut game = PossibilityMatrix::blank(8);
    game.solutions(|s| {
        println!("{}", s);
        true
    });
}
