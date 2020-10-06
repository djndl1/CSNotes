#include <iostream>
#include <stack>
#include <utility>
#include <deque>

using namespace std;

class Card {
    public:
        char rank;
        char suit;

        Card(const string& card_code)
        {
            if (card_code.size() >= 2) {
                rank = card_code[0];
                suit = card_code[1];
            } else {
                rank = suit = '\0';
            }
        }

        Card(string&& card_code)
        {
            if (card_code.size() >= 2) {
                rank = card_code[0];
                suit = card_code[1];
            } else {
                rank = suit = '\0';
            }
        }

    bool operator==(const Card& other) {
        return rank == other.rank or suit == other.suit;
    }

    bool operator==(Card&& other) {
        return rank == other.rank or suit == other.suit;
    }
};

class Pile {
    stack<Card> pile;
    deque<Card> recent_three;

    bool addCard(const Card& card)
    {
        if (recent_three.size() > 0) {
            if (recent_three.back() == card) {

            }
        }
    }
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    while (true) {
        int N = 52;
        while (N--) {
            string tmp{};
            cin >> tmp;


        }
    }
}
