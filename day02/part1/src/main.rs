use serde::Deserialize;

#[derive(Deserialize, Debug, PartialEq)]
enum Shape {
    #[serde(alias = "A")]
    #[serde(alias = "X")]
    Rock,
    #[serde(alias = "B")]
    #[serde(alias = "Y")]
    Paper,
    #[serde(alias = "C")]
    #[serde(alias = "Z")]
    Scissors,
}

#[derive(Deserialize, Debug)]
struct TournamentRound {
    opponent_move: Shape,
    my_move: Shape,
}

impl Shape {
    fn points(&self) -> i64 {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3
        }
    }
}

impl TournamentRound {
    fn points(&self) -> i64 {
        match self {
            TournamentRound { opponent_move: Shape::Rock, my_move: Shape::Paper } => 6 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Rock, my_move: Shape::Scissors } => 0 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Paper, my_move: Shape::Rock } => 0 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Paper, my_move: Shape::Scissors } => 6 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Scissors, my_move: Shape::Rock } => 6 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Scissors, my_move: Shape::Paper } => 0 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Rock, my_move: Shape::Rock } => 3 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Paper, my_move: Shape::Paper } => 3 + self.my_move.points(),
            TournamentRound { opponent_move: Shape::Scissors, my_move: Shape::Scissors } => 3 + self.my_move.points(),
        }
    }
}

fn main() {
    let mut rdr = csv::ReaderBuilder::new()
        .delimiter(b' ')
        .has_headers(false)
        .from_path("./input.csv")
        .unwrap();
    let tournament_rounds = rdr
        .deserialize::<TournamentRound>()
        .collect::<Vec<_>>()
        .into_iter()
        .collect::<Result<Vec<TournamentRound>, _>>()
        .unwrap();
    let my_score = tournament_rounds
        .iter()
        .fold(0, |accumulator, tournament_round| accumulator + tournament_round.points());
    println!("{}", my_score);
}
