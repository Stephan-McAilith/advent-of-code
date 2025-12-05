#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <utility>
#include <algorithm>

unsigned long long ex1(char* inputfile) {
	std::ifstream istream(inputfile);
    std::string row;
    std::vector<std::pair<unsigned long long, unsigned long long>> freshRanges;
    unsigned long long nbValidIngredient = 0;
    unsigned long long rangeStart, rangeEnd;
    char dash;
    for (std::getline(istream, row); !row.empty(); std::getline(istream, row)) {
        std::istringstream rowStream(row);
        rowStream >> rangeStart >> dash >> rangeEnd;
        freshRanges.push_back(std::make_pair(rangeStart, rangeEnd));
    }
    while (std::getline(istream, row)) {
        unsigned long long ingrediendId = stoll(row);
        for (auto it = freshRanges.begin(); it != freshRanges.end(); it++) {
            if (ingrediendId >= it->first && ingrediendId <= it->second) {
                nbValidIngredient += 1;
                break;
            }

        }
    }
    return nbValidIngredient;
}

struct RangeLimit {
    unsigned long long position;
    bool isStart;
};

bool rangeCompare(RangeLimit left, RangeLimit right) {
    if (left.position == right.position)
        return !(right.isStart && !left.isStart);
    return (left.position < right.position);
}

unsigned long long ex2(char* inputfile) {
    std::ifstream istream(inputfile);
    std::string row;

    std::vector<RangeLimit> rangeLimits;
    unsigned long long nbValidIngredient = 0;
    unsigned long long rangeStart, rangeEnd;
    char dash;
    for (std::getline(istream, row); !row.empty(); std::getline(istream, row)) {
        std::istringstream rowStream(row);
        rowStream >> rangeStart >> dash >> rangeEnd;
        rangeLimits.push_back({rangeStart, true});
        rangeLimits.push_back({rangeEnd, false });
    }
    std::sort(rangeLimits.begin(), rangeLimits.end(), rangeCompare);

    unsigned long long totalFreshIngredientId = 0;
    unsigned long long mergeRangeStart = 0;
    int rangeLimitCounter = 0;
    for (auto it = rangeLimits.begin(); it != rangeLimits.end(); it++) {
        if (it->isStart && rangeLimitCounter == 0)
            mergeRangeStart = it->position;
        rangeLimitCounter += it->isStart ? 1 : -1;
        if (rangeLimitCounter == 0)
            totalFreshIngredientId += it->position - mergeRangeStart + 1;
    }
    return totalFreshIngredientId;
}


int main(int ac, char* arg[])
{
    if (ac < 2) {
        std::cout << "Missing input file argument" << std::endl;
        return 1;
    }

    std::cout << "Ex 1: " << ex1(arg[1]) << std::endl;
    std::cout << "Ex 2: " << ex2(arg[1]) << std::endl;
}