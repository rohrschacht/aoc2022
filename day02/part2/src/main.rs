use serde::Deserialize;

#[derive(Deserialize, Debug, PartialEq, Copy, Clone)]
enum Shape {
    #[serde(alias = "A")]
    Rock,
    #[serde(alias = "B")]
    Paper,
    #[serde(alias = "C")]
    Scissors,
}

#[derive(Deserialize, Debug, PartialEq)]
enum Strategy {
    #[serde(alias = "X")]
    Lose,
    #[serde(alias = "Y")]
    Draw,
    #[serde(alias = "Z")]
    Win
}

#[derive(Deserialize, Debug)]
struct TournamentRound {
    opponent_move: Shape,
    my_strategy: Strategy,
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

impl Strategy {
    fn points(&self) -> i64 {
        match self {
            Strategy::Lose => 0,
            Strategy::Draw => 3,
            Strategy::Win => 6
        }
    }

    fn my_move(&self, opponent_move: &Shape) -> Shape {
        match self {
            Strategy::Lose => {
                match opponent_move {
                    Shape::Rock => Shape::Scissors,
                    Shape::Paper => Shape::Rock,
                    Shape::Scissors => Shape::Paper
                }
            }
            Strategy::Draw => {
                *opponent_move
            }
            Strategy::Win => {
                match opponent_move {
                    Shape::Rock => Shape::Paper,
                    Shape::Paper => Shape::Scissors,
                    Shape::Scissors => Shape::Rock
                }
            }
        }
    }

    fn outcome(&self, opponent_move: &Shape) -> i64 {
        self.points() + self.my_move(opponent_move).points()
    }
}

impl TournamentRound {
    fn points(&self) -> i64 {
        self.my_strategy.outcome(&self.opponent_move)
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
