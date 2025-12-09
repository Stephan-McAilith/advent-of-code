#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <utility>
#include <algorithm>
#include <numeric>
#include <set>
#include <map>
#include <utility>

unsigned long long ex1(char *inputfile)
{
    std::ifstream istream(inputfile);
    std::string line;
    std::vector<std::pair<long long, long long>> corners;

    while (std::getline(istream, line)) {
        int x, y;
        char comma;
        std::istringstream lineStream(line);
        lineStream >> x >> comma >> y;
        corners.push_back(std::make_pair(x, y));
    }

    long long biggestSquare = 0;
    for (auto it = corners.begin(); it != corners.end(); it++) {
        for (auto itBis = it; ++itBis != corners.end();)
            biggestSquare = std::max(biggestSquare, (abs(it->first - itBis->first) + 1) * (abs(it->second - itBis->second) + 1));
    }
    return biggestSquare;
}

class SquareFinder {
public:
    SquareFinder(char* inputfile) { parse(inputfile); }
    long long findBiggestSquare() {
        int counter = 0;
        long long biggestSquare = 0;
        for (auto it = corners.begin(); it != corners.end(); it++) {
            for (auto itBis = it; ++itBis != corners.end();) {
                long long squareSize = (abs(it->first - itBis->first) + 1) * (abs(it->second - itBis->second) + 1);
                if (squareSize > biggestSquare
                    && hasNoCornerInside(it->first, it->second, itBis->first, itBis->second)
                    /* && isInGreenTiles(it->first, it->second, itBis->first, itBis->second)*/) {
                    biggestSquare = squareSize;
                }
            }

        }
        return (biggestSquare);
    }

private:
    bool hasNoCornerInside(const long long& x1, const long long& y1, const long long& x2, const long long& y2) {
        long long startX = std::min(x1, x2), endX = std::max(x1, x2), startY = std::min(y1, y2), endY = std::max(y1, y2);
        for (int i = 0; i < corners.size() - 1; i += 1) {
            if (corners[i].first > startX && corners[i].first < endX && corners[i].second > startY && corners[i].second < endY)
                return false;
            if (corners[i].first > startX && corners[i].first < endX && corners[i+1].first > startX && corners[i+1].first < endX) {
                if (corners[i].second <= startY && corners[i + 1].second >= endY || corners[i + 1].second <= startY && corners[i].second >= endY)
                    return false;
            }
            if (corners[i].second > startY && corners[i].second < endY && corners[i + 1].second > startY && corners[i + 1].second < endY) {
                if (corners[i].first <= startX && corners[i + 1].first >= endX || corners[i + 1].first <= startX && corners[i].first >= endX)
                    return false;
            }
        }
        return true;
    }

    void isIn(const std::vector<std::pair<long long, long long>>& sides, int i) {
        for (auto it = sides.begin(); it != sides.end(); it++) {
            if (it->first < i && it->second > i) {
                in = !in;
            }
            else if (it->first == i) {
                if (l = true) {
                    l = false;
                    in = !old;
                }
                else if (r == true) {
                    in = old;
                    r = false;
                }
                else {
                    old = in;
                    in = true;
                    r = true;
                }
            }
            else if (it->second == i) {
                if (r = true) {
                    r = false;
                    in = !old;
                }
                else if (l == true) {
                    in = old;
                    l = false;
                }
                else {
                    old = in;
                    in = true;
                    l = true;
                }
            }
        }
    }

    bool isInGreenTiles(const long long& x1, const long long& y1, const long long& x2, const long long& y2) {
        long long startX = std::min(x1, x2), endX = std::max(x1, x2), startY = std::min(y1, y2), endY = std::max(y1, y2);
        for (long long i = startX + 1; i < endX; i += 1) {
            in = false; r = false; l = false; old = false;
            for (long long j = minY; j <= startY; j += 1)
                isIn(horizontalSides[j], i);
            if (!in)
                return false;
           /* in = false; r = false; l = false; old = false;
            for (long long j = maxY; j >= endY; j -= 1)
                isIn(horizontalSides[j], i);
            if (!in)
                return false;*/
        }

       /* for (long long i = startY + 1; i < endY; i += 1) {
            in = false; r = false; l = false; old = false;
            for (long long j = minX; j <= startX; j += 1)
                isIn(verticalSides[j], i);
            if (!in)
                return false;
            in = false; r = false; l = false; old = false;
            for (long long j = maxX; j >= endX; j -= 1)
                isIn(verticalSides[j], i);
            if (!in)
                return false;
        }*/
        return true;
    }

    void parse(char* inputfile) {
        std::ifstream istream(inputfile);
        std::string line;
       
        long long prevX = -1, prevY = -1;
        while (std::getline(istream, line)) {
            long long x, y;
            char comma;
            std::istringstream lineStream(line);
            lineStream >> x >> comma >> y;
            corners.push_back(std::make_pair(x, y));
            minX = std::min(minX, x);
            minY = std::min(minY, y);
            maxX = std::max(maxX, x);
            maxY = std::max(maxY, y);
            addSide(prevX, prevY, x, y);
            prevX = x;
            prevY = y;
        }
        corners.push_back(std::make_pair(corners[0].first, corners[0].second));

        addSide(prevX, prevY, corners[0].first, corners[0].second);
    }

    void addSide(const long long &x, const long long &y, const long long &x2, const long long &y2) {
        if (x2 == x)
            verticalSides[x].push_back(std::make_pair(std::min(y, y2), std::max(y, y2)));
        if (y2 == y)
            horizontalSides[y].push_back(std::make_pair(std::min(x, x2), std::max(x, x2)));
    }

    long long minX = INT64_MAX, minY = INT64_MAX, maxX = 0, maxY = 0;
    long long prevX = -1, prevY = -1;
    std::vector<std::pair<long long, long long>> corners;
    std::map<long long, std::vector<std::pair<long long, long long>>> horizontalSides;
    std::map<long long, std::vector<std::pair<long long, long long>>> verticalSides;
    bool in = false, r = false, l = false, old = false;

};

unsigned long long ex2(char *inputfile)
{
    SquareFinder squareFinder(inputfile);
    return squareFinder.findBiggestSquare();
}

int main(int ac, char *arg[])
{
    if (ac < 2)
    {
        std::cout << "Missing input file argument" << std::endl;
        return 1;
    }

    std::cout << "Ex 1: " << ex1(arg[1]) << std::endl;
    std::cout << "Ex 2: " << ex2(arg[1]) << std::endl;
}