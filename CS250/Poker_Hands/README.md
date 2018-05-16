# CS 250 - Assignment 3  
Create a program to shuffle and deal a deck of cards.  The program should consist of class Card, class DeckOfCards, and a driver program.

Class Card should provide the following:

+ Data members face and suit of type int
+ A constructor that receives two ints representing the face and suit and uses them to initialize the data members
+ Two static arrays of strings representing the faces and suits
+ A `toString` function that returns the Card as a string in the form “face of suit”.  You can use the + operator to concatenate strings.

Class DeckOfCards should contain the following:

+ an array of Cards named `deck` to store the Cards
+ An integer currentCard representing the next card to deal.
+ A default constructor that initializes the Cards in the deck.
+ A shuffle function that shuffles the Cards in the deck. The shuffle algorithm should iterate through the array of Cards. For each Card, randomly select another Card in the deck and swap the two Cards.
+ A dealCard function that returns the next Card object from the deck.
+ A moreCards function that returns a bool value indicating whether there are more Cards to deal.

Have the program deal  a five-card hand and include methods to determine whether the hand contains any of the following:

1. a pair
2. two pairs
3. three of a kind
4. four of a kind
5. a flush (five cards of the same suit)
6. a straight (five cards in order)

Finally, the driver program should deal two hands and determine which hand is best (using the rankings given above.)

Also, note:  if you think of a different way to organize or factor the structures (such as using an enum for face and suit instead of an int linked to an array of strings) or to implement things like the shuffling, please feel free to do so!  Use all your experience and implement this in the best way possible.
