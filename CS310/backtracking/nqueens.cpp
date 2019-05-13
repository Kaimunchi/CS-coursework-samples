/**
 * nqueens by backtracking
 * @author original author Jon Beck
 * @author modified by Brandon Ingli
 * @version 17 April 2019
 */
#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <vector>
#include "matrix.h"
using namespace std;

/**
 * Checks to see if the current placement of queens on
 * the board is ok.  It is ok if no queen attacks another queen.  A
 * return of true means no conflict.  A return of false means at least
 * two queens conflict.  The function iterates through the board by 
 * row and col (in that order). If there is a queen at a given position, the 
 * function calculates a value indicating what "up" and "down" diagonals the 
 * cell lies on, then checks if a queen has been seen on the row, col, or 
 * either diagonal. If so, a flag is raised and the loops terminate. If not, 
 * the function remembers what row/col/diags this queen was in and continues 
 * iterating to find the next queen.
 * 
 * @param board the n x n chessboard
 * @return true if there are no conflicts; false if there is any conflict
 */
bool test(const Matrix<bool> & board);

/**
 * output an ASCII art horizontal line with plus
 * signs where columns will intersect
 */
void hr(unsigned cols);

/**
 * dump a chessboard to std output
 * @param board the board whose arrangement to dump
 */
void printBoard(const Matrix<bool> & board);

/**
 * backtrack recursively. When called, k queens have already been
 * placed on the board in columns 0 .. k - 1.  We're trying to place the
 * next queen in column k.
 * @param k the column in which to place the current queen
 * @param board the board with the previous queens already on it
 * @param nodes_visited counter of nodes visited in search tree
 * @param row_occupied vector indicating if queen is placed in a given row
 */
void backtrack(size_t k, Matrix<bool> & board, size_t & nodes_visited, 
  vector<bool> & row_occupied);


int main(int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << "Usage: " << argv[0] << " n" << endl;
    cout << "       where n is the number of queens to place" << endl;
    cout << "       on an n x n chessboard, with 0 < n < 26" << endl;
    return 2;
  }

  size_t n {static_cast<unsigned>(stoul(argv[1]))};
  assert (n > 0 && n < 26);

  // initialize a board with no queens
  Matrix<bool> board(n, n);
  for (size_t row = 0; row < n; row++)
  {
    for (size_t col = 0; col < n; col++)
    {
      board.at(row, col) = false;
    }
  }

  size_t nodes_visited {1};
  vector<bool> row_occupied (board.numrows(), false);

  // start with column 0
  backtrack(0, board, nodes_visited, row_occupied);
  cout << "No solution" << endl;
  cerr << nodes_visited << endl;
  return 1;
}

bool test(const Matrix< bool > & board)
{
  size_t n = board.numrows();
  vector<bool> row_occupied (n, false);
  vector<bool> col_occupied (n, false);
  vector<bool> up_diag_occupied(2 * n, false);
  vector<bool> dn_diag_occupied(2 * n, false);
  bool ok = true;

  // let row and col range over the entire board while it is still valid
  size_t row {0};
  while(ok && row < n)
  {
    size_t col {0};
    while (ok && col < n)
    {
      //We only need to do work if there's a queen here
      if(board.at(row, col))
      {
        //Determine which diagonals this queen sits on
        size_t up_diag_pos {n - col + row};
        size_t dn_diag_pos {row + col + 1};

        //See if a queen has appeared on this row/col/diag earlier
        if(row_occupied.at(row) || col_occupied.at(col) || 
            up_diag_occupied.at(up_diag_pos) || 
            dn_diag_occupied.at(dn_diag_pos))
        {
          // Queen present in the same row, col, or diag; Fail
          ok = false;
        }
        else
        {
          // Remember that this row, col, and diags have a queen
          row_occupied.at(row) = true;
          col_occupied.at(col) = true;
          up_diag_occupied.at(up_diag_pos) = true;
          dn_diag_occupied.at(dn_diag_pos) = true;
        }
      }
      col++;
    }
    row++;
  }
  return ok;
}

void backtrack(size_t k, Matrix< bool > & board, size_t & nodes_visited, 
  vector<bool> & row_occupied)
{

  // are we done?
  if (k == board.numrows())
  {
    // if so, report and exit
    printBoard(board);
    cerr << nodes_visited << endl;
    exit(0);
  }

  // try each row in turn, for this column
  for (size_t row = 0; row < board.numrows(); row++)
  {

    //Only visit nodes for rows where there is no queen already
    if (!row_occupied.at(row))
    {
      nodes_visited++;

      // put a queen here
      board.at(row, k) = true;
      row_occupied.at(row) = true;

      // did that cause a conflict?
      if (test(board))
      {
        // keep going
        backtrack(k + 1, board, nodes_visited, row_occupied);
      }
      // if that didn't work, un-try the current attempt
      board.at(row, k) = false;
      row_occupied.at(row) = false;
    }
  }
}

void printBoard(const Matrix<bool> & board)
{
  hr(static_cast<unsigned>(board.numrows()));
  for (size_t row = 0; row < board.numrows(); row++)
  {
    cout << ' ' << setw(2) << board.numrows() - row << " |";
    for (size_t col = 0; col < board.numrows(); col++)
    {
      if (board.at(row, col))
        cout << " X |";
      else
        cout << "   |";
    }
    cout << endl;
    hr(static_cast<unsigned>(board.numrows()));
  }

  cout << "     ";
  for (size_t col = 0; col < board.numrows(); col++)
  {
    cout << ' ' << static_cast<char>('a' + col) << "  ";
  }
  cout << endl;
}

void hr(unsigned cols)
{
  cout << "    +";
  for (unsigned col = 0; col < cols; col++)
    cout << "---+";
  cout << endl;
}
