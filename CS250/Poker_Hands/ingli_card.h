/* Brandon Ingli
CS 250 Assignment 3
Spring 2018 */

#ifndef INGLI_CARD
#define INGLI_CARD

#include <string>
using namespace std;

class Card{
  public:
    enum class Face : char {Two = 2, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace};
    enum class Suit : char {Diamonds, Clubs, Hearts, Spades};

    Card(Face f, Suit s) : face(f), suit(s) {};
    Card() : face(Face::Ace), suit(Suit::Spades) {};

    string toString() const;
    Face getFace() const;
    string getFaceStr() const;
    Suit getSuit() const;
    string getSuitStr() const;

  private:
    Face face;
    Suit suit;
};
bool operator>(const Card&, const Card&);
bool operator<(const Card&, const Card&);
ostream& operator<<(ostream&, const Card&);

#endif
