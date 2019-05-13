/**
 * nqueens by backtracking
 * @author original author Jon Beck
 * @author modified by Your Name Here
 * @version your date here
 */
#include <cassert>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include "matrix.h"
using namespace std;

/**
 * Checks to see if the current placement of queens on
 * the board is ok.  It is ok if no queen attacks another queen.  A
 * return of true means no conflict.  A return of false means at least
 * two queens conflict.  This function is designed for clarity, NOT
 * efficiency.
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
 */
void backtrack(size_t k, Matrix<bool> & board, size_t & nodes_visited);


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

  // start with column 0
  size_t nodes_visited {1};
  backtrack(0, board, nodes_visited);
  cout << "No solution" << endl;
  cerr << nodes_visited << endl;
  return 1;
}

bool test(const Matrix< bool > & board)
{
  size_t n = board.numrows();
  bool ok = true;
  
  // let row and col range over the entire board
  for (size_t row = 0; row < n; row++)
  {
    for (size_t col = 0; col < n; col++)
    {
      // if row and col designate a queen's position, see if it
      // conflicts with any other queen

      // let tryrow and trycol range over the entire board, looking
      // for a conflicting queen
      for(size_t tryrow = 0; tryrow < n; tryrow++)
      {
        for (size_t trycol = 0; trycol < n; trycol++)
        {
          // if there's a queen at both row,col and tryrow,trycol
          if (board.at(row, col) && board.at(tryrow, trycol) &&
              // and it's not the same spot
              !(row == tryrow && col == trycol))
          {
            // first check if they're in the same column
            if (col == trycol)
            {
              ok = false;
            }
            // now check if they're in the sam row
            if (row == tryrow)
            {
              ok = false;
            }
            // now see if they're in the same "up" diagonal
            if (n - col + row == n - trycol + tryrow)
            {
              ok = false;
            }
            // and finally the same down diag
            if (row + col + 1 == tryrow + trycol + 1)
            {
              ok = false;
            }
          }
        }
      }
    }
  }
  return ok;
}

void backtrack(size_t k, Matrix< bool > & board, size_t & nodes_visited)
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
    nodes_visited++;

    // put a queen here
    board.at(row, k) = true;

    // did that cause a conflict?
    if (test(board))
    {
      // keep going
      backtrack(k + 1, board, nodes_visited);
    }
    // if that didn't work, un-try the current attempt
    board.at(row, k) = false;
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
