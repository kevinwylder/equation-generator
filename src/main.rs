use nerdle::PossibilityMatrix;

fn main() {
    let game = PossibilityMatrix::<8>::blank();
    game.solutions(|s| {
        println!("{}", s);
        true
    });
}
