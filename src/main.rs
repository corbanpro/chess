use colored::ColoredString;
use colored::Colorize;
use PieceType::*;
use Team::*;
fn get_piece_char(piece: &Option<Piece>) -> ColoredString {
    if piece.is_none() {
        return " ".into();
    }
    let char = match piece.unwrap().piece_type() {
        King => "♚",
        Queen => "♛",
        Rook => "♜",
        Bishop => "♝",
        Knight => "♞",
        Pawn => "♟",
    };
    match piece.unwrap().team() {
        Black => char.red(),
        White => char.blue(),
    }
}
fn get_default_board() -> [[Option<Piece>; 8]; 8] {
    [
        [
            Some(Piece::new(Rook, White)),
            Some(Piece::new(Knight, White)),
            Some(Piece::new(Bishop, White)),
            Some(Piece::new(Queen, White)),
            Some(Piece::new(King, White)),
            Some(Piece::new(Bishop, White)),
            Some(Piece::new(Knight, White)),
            Some(Piece::new(Rook, White)),
        ],
        [Some(Piece::new(Pawn, White)); 8],
        [None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None],
        [Some(Piece::new(Pawn, Black)); 8],
        [
            Some(Piece::new(Rook, Black)),
            Some(Piece::new(Knight, Black)),
            Some(Piece::new(Bishop, Black)),
            Some(Piece::new(Queen, Black)),
            Some(Piece::new(King, Black)),
            Some(Piece::new(Bishop, Black)),
            Some(Piece::new(Knight, Black)),
            Some(Piece::new(Rook, Black)),
        ],
    ]
}
#[derive(Clone, Copy)]
struct Game {
    board: [[Option<Piece>; 8]; 8],
    white_king_moved: bool,
    white_rook_a_moved: bool,
    white_rook_h_moved: bool,
    black_king_moved: bool,
    black_rook_a_moved: bool,
    black_rook_h_moved: bool,
    en_passant_pawn: Option<Square>,
}
#[derive(Debug, Clone, PartialEq, Copy)]
struct Piece {
    piece_type: PieceType,
    team: Team,
}
#[derive(Debug, Clone, PartialEq, Copy)]
enum Team {
    White,
    Black,
}
#[derive(Debug, Clone, PartialEq, Copy)]
enum PieceType {
    King,
    Queen,
    Bishop,
    Knight,
    Rook,
    Pawn,
}
#[derive(Debug, PartialEq, Clone, Copy)]
struct Square {
    rank: usize,
    file: usize,
}
impl Piece {
    fn new(piece_type: PieceType, team: Team) -> Piece {
        Piece { piece_type, team }
    }
    fn team(&self) -> Team {
        self.team
    }
    fn piece_type(&self) -> PieceType {
        self.piece_type
    }
    fn is_white(&self) -> bool {
        self.team() == White
    }
    fn is_king(&self) -> bool {
        self.piece_type() == King
    }
    fn is_pawn(&self) -> bool {
        self.piece_type() == Pawn
    }
}
impl Square {
    fn rank(&self) -> usize {
        self.rank
    }
    fn file(&self) -> usize {
        self.file
    }
    fn new(rank: usize, file: usize) -> Square {
        Square { rank, file }
    }
    fn from_usize(rank: usize, file: usize) -> Result<Square, String> {
        if rank > 7 || file > 7 {
            return Err("Error: Invalid Square".to_string());
        };
        return Ok(Square { rank, file });
    }
    fn from_i32(rank: i32, file: i32) -> Result<Square, String> {
        if rank > 7 || file > 7 || rank < 0 || file < 0 {
            return Err("Error: Invalid Square".to_string());
        };
        let rank = rank as usize;
        let file = file as usize;
        return Ok(Square { rank, file });
    }
    fn from_coords(coords: &str) -> Result<Square, String> {
        if coords.chars().count() != 2 {
            return Err("Error: Invalid Coordinates".to_string());
        }
        let mut chars = coords.chars();
        let file_str = chars.next().unwrap();
        let rank_str = chars.next().unwrap();
        let file_strs = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
        let file = match file_strs.iter().position(|&ele| ele == file_str) {
            Some(file) => file,
            None => return Err("Error: Invalid Coordinates".to_string()),
        };
        let rank = match rank_str.to_string().parse::<usize>() {
            Ok(rank) => rank - 1,
            Err(_) => return Err("Error: Invalid Coordinates".to_string()),
        };
        Square::from_usize(rank, file)
    }
    fn rank_diff(&self, target_sq: Square) -> usize {
        (self.rank() as i32 - target_sq.rank() as i32).abs() as usize
    }
    fn file_diff(&self, target_sq: Square) -> usize {
        (self.file() as i32 - target_sq.file() as i32).abs() as usize
    }
}
impl Game {
    fn init() -> Game {
        Game {
            board: get_default_board(),
            white_king_moved: false,
            white_rook_a_moved: false,
            white_rook_h_moved: false,
            black_king_moved: false,
            black_rook_a_moved: false,
            black_rook_h_moved: false,
            en_passant_pawn: None,
        }
    }
    fn get_board(&self) -> [[Option<Piece>; 8]; 8] {
        self.board
    }
    fn get_piece(&self, square: Square) -> Option<Piece> {
        self.get_board()[square.rank()][square.file()]
    }
    fn find_team_pieces(&self, team: Team) -> Vec<Square> {
        let mut pieces = vec![];
        for (rank_index, rank) in self.get_board().iter().enumerate() {
            for (file_index, piece) in rank.iter().enumerate() {
                if piece.is_some() && piece.unwrap().team() == team {
                    pieces.push(Square::new(rank_index, file_index))
                }
            }
        }
        pieces
    }
    fn find_king(&self, team: Team) -> Square {
        for (rank_index, rank) in self.get_board().iter().enumerate() {
            for (file_index, piece) in rank.iter().enumerate() {
                if piece.is_some() && piece.unwrap().is_king() && piece.unwrap().team() == team {
                    return Square::new(rank_index, file_index);
                }
            }
        }
        panic!("Didn't find {:?} king", team)
    }
    fn last_rank_pawn_index(&self, is_white_turn: bool) -> Option<usize> {
        let rank_index = if is_white_turn { 7 } else { 0 };
        self.get_board()[rank_index]
            .iter()
            .position(|piece| piece.is_some() && piece.unwrap().is_pawn())
    }
    fn replace_last_rank_pawn(&mut self, is_white_turn: bool, file: usize, piece_type: PieceType) {
        let rank_index = if is_white_turn { 7 } else { 0 };
        let team = if is_white_turn { White } else { Black };
        self.board[rank_index][file] = Some(Piece::new(piece_type, team));
    }

    fn is_checkmate(&self, is_white_turn: bool) -> bool {
        let loser_team = if is_white_turn { Black } else { White };
        let loser_piece_squares = self.find_team_pieces(loser_team);
        for loser_piece_sq in loser_piece_squares {
            if self.get_legal_moves(is_white_turn, loser_piece_sq).len() > 0 {
                return false;
            }
        }
        return true;
    }
    fn white_in_check(&self) -> bool {
        let black_piece_squares = self.find_team_pieces(Black);
        let white_king_sq = &self.find_king(White);
        let is_white_turn = false;
        for black_piece_sq in black_piece_squares {
            if self
                .get_possible_moves(is_white_turn, black_piece_sq)
                .contains(white_king_sq)
            {
                return true;
            };
        }
        false
    }
    fn black_in_check(&self) -> bool {
        let white_piece_squares = self.find_team_pieces(White);
        let black_king_sq = &self.find_king(Black);
        let is_white_turn = true;
        for white_piece_sq in white_piece_squares {
            if self
                .get_possible_moves(is_white_turn, white_piece_sq)
                .contains(black_king_sq)
            {
                return true;
            };
        }
        false
    }
    fn get_diagonal_moves(&self, start_sq: Square) -> Vec<Square> {
        let mut potential_moves = vec![];
        let start_rank = start_sq.rank() as i32;
        let start_file = start_sq.file() as i32;
        //move forward-right
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank + distance, start_file + distance);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        //move forward-left
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank + distance, start_file - distance);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        //move backward-right
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank - distance, start_file + distance);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        //move backward-left
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank - distance, start_file - distance);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        potential_moves
    }
    fn get_straight_moves(&self, start_sq: Square) -> Vec<Square> {
        let mut potential_moves = vec![];
        let start_rank = start_sq.rank() as i32;
        let start_file = start_sq.file() as i32;
        // move right
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank + distance, start_file);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        //move left
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank - distance, start_file);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        // move forward
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank, start_file + distance);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        //move backward
        for distance in 1..8 {
            let new_sq = Square::from_i32(start_rank, start_file - distance);
            if new_sq.is_ok() {
                potential_moves.push(new_sq.clone().unwrap());
            }
            if new_sq.is_err() || self.get_piece(new_sq.unwrap()).is_some() {
                break;
            }
        }
        potential_moves
    }
    fn get_king_moves(&self, start_sq: Square, is_white_turn: bool) -> Vec<Square> {
        let start_rank = start_sq.rank() as i32;
        let start_file = start_sq.file() as i32;

        let mut potential_moves = vec![];
        for rank_movement in [-1, 0, 1] {
            for file_movement in [-1, 0, 1] {
                if rank_movement == 0 && file_movement == 0 {
                    continue;
                }
                let new_rank = start_rank + rank_movement;
                let new_file = start_file + file_movement;
                let new_sq = Square::from_i32(new_rank, new_file);
                if new_sq.is_ok() {
                    potential_moves.push(new_sq.unwrap())
                }
            }
        }

        // castle
        if is_white_turn && !self.white_king_moved {
            if !self.white_rook_a_moved
                && self.get_piece(Square::new(0, 1)).is_none()
                && self.get_piece(Square::new(0, 2)).is_none()
                && self.get_piece(Square::new(0, 3)).is_none()
            {
                potential_moves.push(Square::new(0, 1));
            }
            if !self.white_rook_h_moved
                && self.get_piece(Square::new(0, 5)).is_none()
                && self.get_piece(Square::new(0, 6)).is_none()
            {
                potential_moves.push(Square::new(0, 6));
            }
        } else if !is_white_turn && !self.black_king_moved {
            if !self.black_rook_a_moved
                && self.get_piece(Square::new(7, 1)).is_none()
                && self.get_piece(Square::new(7, 2)).is_none()
                && self.get_piece(Square::new(7, 3)).is_none()
            {
                potential_moves.push(Square::new(7, 1));
            }
            if !self.black_rook_h_moved
                && self.get_piece(Square::new(7, 5)).is_none()
                && self.get_piece(Square::new(7, 6)).is_none()
            {
                potential_moves.push(Square::new(7, 6));
            }
        }

        potential_moves
    }
    fn get_queen_moves(&self, start_sq: Square) -> Vec<Square> {
        let mut diagonal_moves = self.get_diagonal_moves(start_sq);
        let mut straight_moves = self.get_straight_moves(start_sq);
        diagonal_moves.append(&mut straight_moves);
        diagonal_moves
    }
    fn get_bishop_moves(&self, start_sq: Square) -> Vec<Square> {
        self.get_diagonal_moves(start_sq)
    }
    fn get_knight_moves(&self, start_sq: Square) -> Vec<Square> {
        let mut potential_moves = vec![];
        let start_rank = start_sq.rank() as i32;
        let start_file = start_sq.file() as i32;

        for rank_diff in [-2_i32, -1, 1, 2] {
            for file_diff in [-2_i32, -1, 1, 2] {
                if rank_diff.abs() + file_diff.abs() == 3 {
                    let new_sq = Square::from_i32(start_rank + rank_diff, start_file + file_diff);
                    if new_sq.is_ok() {
                        potential_moves.push(new_sq.unwrap())
                    }
                }
            }
        }

        potential_moves
    }
    fn get_rook_moves(&self, start_sq: Square) -> Vec<Square> {
        self.get_straight_moves(start_sq)
    }
    fn get_pawn_moves(&self, is_white_turn: bool, start_sq: Square) -> Vec<Square> {
        let mut potential_moves = vec![];

        let start_rank = start_sq.rank() as i32;
        let start_file = start_sq.file() as i32;
        let unmoved_rank = if is_white_turn { 1 } else { 6 };
        let move_direction = if is_white_turn { 1 } else { -1 };
        let single_move_rank = start_rank + move_direction;
        let double_move_rank = single_move_rank + move_direction;

        if self
            .get_piece(Square::from_i32(single_move_rank, start_file).unwrap())
            .is_none()
        {
            potential_moves.push(Square::from_i32(single_move_rank, start_file).unwrap());
            if start_rank == unmoved_rank
                && self
                    .get_piece(Square::from_i32(double_move_rank, start_file).unwrap())
                    .is_none()
            {
                potential_moves.push(Square::from_i32(double_move_rank, start_file).unwrap());
            }
        }

        if start_file != 7 {
            let right_attack_sq = Square::from_i32(single_move_rank, start_file + 1).unwrap();
            if self.get_piece(right_attack_sq).is_some() {
                potential_moves.push(right_attack_sq);
            }
        }
        if start_file != 0 {
            let left_attack_sq = Square::from_i32(single_move_rank, start_file - 1).unwrap();
            if self.get_piece(left_attack_sq).is_some() {
                potential_moves.push(left_attack_sq);
            }
        }

        // en passant
        if self.en_passant_pawn.is_some()
            && start_sq.rank_diff(self.en_passant_pawn.unwrap()) == 0
            && start_sq.file_diff(self.en_passant_pawn.unwrap()) == 1
        {
            potential_moves.push(Square::new(
                single_move_rank as usize,
                self.en_passant_pawn.unwrap().file(),
            ));
        }

        potential_moves
    }
    fn get_possible_moves(&self, is_white_turn: bool, start_sq: Square) -> Vec<Square> {
        let start_piece = self.get_piece(start_sq);

        if start_piece.is_none() {
            return vec![];
        }
        let start_piece = start_piece.unwrap();
        if start_piece.is_white() != is_white_turn {
            return vec![];
        }

        let mut possible_moves: Vec<Square> = match start_piece.piece_type() {
            PieceType::King => self.get_king_moves(start_sq, is_white_turn),
            PieceType::Queen => self.get_queen_moves(start_sq),
            PieceType::Bishop => self.get_bishop_moves(start_sq),
            PieceType::Knight => self.get_knight_moves(start_sq),
            PieceType::Rook => self.get_rook_moves(start_sq),
            PieceType::Pawn => self.get_pawn_moves(is_white_turn, start_sq),
        };

        possible_moves.retain(|target_sq| {
            let target_piece = self.get_piece(*target_sq);

            if target_piece.is_some() && target_piece.unwrap().team() == start_piece.team() {
                return false;
            } else {
                return true;
            }
        });

        possible_moves
    }
    fn get_legal_moves(&self, is_white_turn: bool, start_sq: Square) -> Vec<Square> {
        let mut legal_moves = self.get_possible_moves(is_white_turn, start_sq);

        legal_moves.retain(|possible_move| {
            // cannot move into check
            let test_game = self.move_piece_test(start_sq, *possible_move);

            if (is_white_turn && test_game.white_in_check())
                || (!is_white_turn && test_game.black_in_check())
            {
                return false;
            }
            true
        });

        legal_moves
    }
    fn move_piece(
        &mut self,
        is_white_turn: bool,
        start_sq: Square,
        target_sq: Square,
    ) -> Result<(), String> {
        if !self
            .get_legal_moves(is_white_turn, start_sq)
            .contains(&target_sq)
        {
            return Err("Error: Invalid Move".to_string());
        }
        let piece = self.board[start_sq.rank()][start_sq.file()].unwrap();

        // castle
        if piece.is_king() && start_sq.file_diff(target_sq) > 1 {
            if (is_white_turn && self.white_in_check()) || (!is_white_turn && self.black_in_check())
            {
                return Err("You cannot castle while in check".to_string());
            }

            if target_sq.file() == 6 {
                let test_move = self.move_piece_test(start_sq, Square::new(target_sq.rank(), 5));

                if (is_white_turn && test_move.white_in_check())
                    || (!is_white_turn && test_move.black_in_check())
                {
                    return Err("You cannot castle through check".to_string());
                }
            }

            if target_sq.file() == 1 {
                let test_move_1 = self.move_piece_test(start_sq, Square::new(target_sq.rank(), 2));
                let test_move_2 = self.move_piece_test(start_sq, Square::new(target_sq.rank(), 3));

                if (is_white_turn && (test_move_1.white_in_check() || test_move_2.white_in_check()))
                    || (!is_white_turn
                        && (test_move_1.black_in_check() || test_move_2.black_in_check()))
                {
                    return Err("You cannot castle through check".to_string());
                }
            }

            let rook_start_file;
            let rook_end_file;
            if target_sq.file() == 6 {
                rook_start_file = 7;
                rook_end_file = 5;
            } else {
                rook_start_file = 0;
                rook_end_file = 2;
            }
            let rook = self.board[target_sq.rank()][rook_start_file];
            self.board[target_sq.rank()][rook_start_file] = None;
            self.board[target_sq.rank()][rook_end_file] = rook
        }

        // en passant
        if piece.is_pawn()
            && self.get_piece(target_sq).is_none()
            && start_sq.file() != target_sq.file()
        {
            self.board[start_sq.rank()][target_sq.file()] = None;
        }
        // execute move
        self.board[start_sq.rank()][start_sq.file()] = None;
        self.board[target_sq.rank()][target_sq.file()] = Some(piece);

        // update moved pieces & en passant
        match (start_sq.rank(), start_sq.file()) {
            (0, 4) => self.white_king_moved = true,
            (0, 0) => self.white_rook_a_moved = true,
            (0, 7) => self.white_rook_h_moved = true,
            (7, 4) => self.black_king_moved = true,
            (7, 0) => self.black_rook_a_moved = true,
            (7, 7) => self.black_rook_h_moved = true,
            _ => {}
        }
        if piece.is_pawn() && start_sq.rank_diff(target_sq) == 2 {
            self.en_passant_pawn = Some(target_sq);
        } else {
            self.en_passant_pawn = None;
        }

        Ok(())
    }
    fn move_piece_test(&self, start_sq: Square, target_sq: Square) -> Game {
        let mut test_game = self.clone();
        let piece = self.board[start_sq.rank][start_sq.file];
        test_game.board[start_sq.rank][start_sq.file] = None;
        test_game.board[target_sq.rank][target_sq.file] = piece;
        test_game
    }
    fn display_board(&self, is_whites_turn: bool) {
        println!("   +----+----+----+----+----+----+----+----+");
        let mut board = self.get_board().clone();

        if is_whites_turn {
            board.reverse();
        }

        for (index, rank) in board.iter().enumerate() {
            let rank_label = if is_whites_turn { 8 - index } else { index + 1 };
            print!("{}  ", rank_label);
            let mut rank = rank.to_owned();
            if !is_whites_turn {
                rank.reverse();
            }
            for square in rank.iter() {
                print!("| {}  ", get_piece_char(square))
            }
            println!("|\n   +----+----+----+----+----+----+----+----+");
        }
        if is_whites_turn {
            println!("     a    b    c    d    e    f    g    h\n")
        } else {
            println!("     h    g    f    e    d    c    b    a\n")
        }
    }
}

fn parse_coords(input: &str) -> Result<(Square, Square), String> {
    let coords: Vec<&str> = input.split(" ").collect();
    if coords.len() != 2 {
        return Err("Error: Must input 2 coordinates".to_string());
    }
    let start_sq = Square::from_coords(coords[0]);
    let target_sq = Square::from_coords(coords[1]);

    let (start_sq, target_sq) = match (start_sq, target_sq) {
        (Ok(start_sq), Ok(target_sq)) => (start_sq, target_sq),
        _ => return Err("Invalid Coordinates".to_string()),
    };

    Ok((start_sq, target_sq))
}

fn handle_pawn_on_last_rank(game: &mut Game, is_whites_turn: bool, pawn_file: usize) {
    loop {
        println!("Congrats! You got a pawn to the last rank. Which piece would you like:\na) Queen\nb) Rook\nc) Bishop\nd) Knight");
        let mut input = "".to_string();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {}
            Err(err) => {
                println!("Error reading input: {}", err);
                continue;
            }
        }
        input = input.trim().to_string();
        let replacement_piece = match &input[..] {
            "a" => PieceType::Queen,
            "b" => PieceType::Rook,
            "c" => PieceType::Bishop,
            "d" => PieceType::Knight,
            _ => {
                println!("{}", "Input Error: Pick from the listed options".red());
                continue;
            }
        };
        game.replace_last_rank_pawn(is_whites_turn, pawn_file, replacement_piece);
        break;
    }
}

fn main() {
    let mut game = Game::init();
    let mut is_whites_turn = true;
    let mut error_message: Option<String> = None;

    let white_lost = loop {
        game.display_board(is_whites_turn);
        if let Some(error_message) = error_message {
            println!("{}\n", format!("{error_message}").red().bold());
        }
        println!(
            "{}, your turn! Enter the coordinate of the piece you want to move followed by the coordinate of the target square",
            if is_whites_turn { "Blue" } else { "Red" }
        );

        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_) => {}
            Err(err) => {
                error_message = Some(format!("Error reading input: {}", err));
                continue;
            }
        }
        input = input.trim().to_string();
        if input == "q" {
            break is_whites_turn;
        }

        let (start_sq, target_sq) = match parse_coords(&input) {
            Ok(res) => res,
            Err(err) => {
                error_message = Some(err);
                continue;
            }
        };

        if let Err(e) = game.move_piece(is_whites_turn, start_sq, target_sq) {
            error_message = Some(format!("{e}"));
            continue;
        }

        let last_rank_pawn = game.last_rank_pawn_index(is_whites_turn);
        if last_rank_pawn.is_some() {
            handle_pawn_on_last_rank(&mut game, is_whites_turn, last_rank_pawn.unwrap())
        }

        if is_whites_turn && game.black_in_check() {
            if game.is_checkmate(is_whites_turn) {
                break !is_whites_turn;
            }
            error_message = Some("Red, you're in check!".to_string());
        } else if !is_whites_turn && game.white_in_check() {
            if game.is_checkmate(is_whites_turn) {
                break !is_whites_turn;
            }
            error_message = Some("Blue, you're in check!".to_string());
        } else {
            error_message = None;
        }

        is_whites_turn = !is_whites_turn;
    };

    game.display_board(is_whites_turn);

    let winner = if white_lost { "Red" } else { "Blue" };
    println!("\nCongratulations {}!!", winner);

    println!("Thanks for playing!!")
}

// TODO checkmate is broken
