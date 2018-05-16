/* Brandon Ingli
CS 250 Assignment 3
Spring 2018 */

#ifndef INGLI_DECK
#define INGLI_DECK

#include <string>
#include <cstdlib>
#include <ctime>
#include "ingli_card.h"
using namespace std;

enum ShuffleState{SHUFFLE = true, NO_SHUFFLE = false};

class DeckOfCards{
  public:
    DeckOfCards(bool);
    DeckOfCards();

    friend ostream& operator<<(ostream& os, const DeckOfCards& rhs);

    void reloadDeck();
    bool isEmpty();
    Card dealCard();
    void shuffle();

  private:
    Card deck[52];
    int curr_card;
    bool shuffleOnReload;
    void swap(int, int);
};

#endif
