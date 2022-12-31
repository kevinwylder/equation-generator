use nerdle::PossibilityMatrix;

fn main() {
    let game = PossibilityMatrix::blank();
    game.solutions(|s| {
        println!("{}", s);
        true
    });
}
