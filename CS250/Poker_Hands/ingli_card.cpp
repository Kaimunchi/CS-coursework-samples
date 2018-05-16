/* Brandon Ingli
CS 250 Assignment 3
Spring 2018 */

#include "ingli_card.h"
#include <string>
using namespace std;

bool operator<(const Card& lhs, const Card &rhs){
  return lhs.getFace() < rhs.getFace();
}
bool operator>(const Card& lhs, const Card &rhs){
  return lhs.getFace() > rhs.getFace();
}
ostream& operator<<(ostream& os, const Card &rhs){
  os << "|" + rhs.toString() + "|";
  return os;
}

string Card::toString() const{
  return getFaceStr() + " Of " + getSuitStr();
}

Card::Face Card::getFace() const{
  return face;
}

string Card::getFaceStr() const{
  switch(face){
    case Face::Ace: return "Ace";
    case Face::Jack: return "Jack";
    case Face::Queen: return "Queen";
    case Face::King: return "King";
    default: return to_string((int) face);
  }
}

Card::Suit Card::getSuit() const{
  return suit;
}

string Card::getSuitStr() const{
  switch(suit){
    case Suit::Diamonds: return "Diamonds";
    case Suit::Clubs: return "Clubs";
    case Suit::Hearts: return "Hearts";
    case Suit::Spades: return "Spades";
    default: return "INVALID";
  }
}
