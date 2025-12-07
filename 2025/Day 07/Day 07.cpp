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


unsigned long long ex1(char* inputfile) {
	std::ifstream istream(inputfile);
    std::string row;
    std::set<unsigned long long> tachyonBeams;
    unsigned long long tachyonSplit = 0;

    std::getline(istream, row);
    for (int i = 0; i < row.size(); i += 1) {
        if (row[i] == 'S') {
            tachyonBeams.insert(i);
            break;
        }
    }

    while (std::getline(istream, row)) {
        std::set<unsigned long long> newtachyonBeams;
        for (auto it = tachyonBeams.begin(); it != tachyonBeams.end();) {
            if (row[*it] == '^') {
                newtachyonBeams.insert(*it + 1);
                newtachyonBeams.insert(*it - 1);
                it = tachyonBeams.erase(it);
                tachyonSplit += 1;
            }
            else
                it++;
        }
        newtachyonBeams.merge(tachyonBeams);
        tachyonBeams = newtachyonBeams;
    }
    return tachyonSplit;
}
typedef std::pair<unsigned long long, unsigned long long> tachyon_posision_t;
typedef std::map<tachyon_posision_t, unsigned long long> path_map_t;

unsigned long long exploreQuantumManifold(path_map_t& exploredPath, std::vector<std::string>& tachyonManifold, tachyon_posision_t pos) {
    if (exploredPath.find(pos) != exploredPath.end())
        return exploredPath[pos];

    int y = std::get<0>(pos), x = std::get<1>(pos);
    if (y >= tachyonManifold.size()) return 1;

    unsigned long long pathAhead = 0;
    if (tachyonManifold[y][x] == '^') {
        pathAhead += exploreQuantumManifold(exploredPath, tachyonManifold, std::make_pair(y + 1, x-1));
        pathAhead += exploreQuantumManifold(exploredPath, tachyonManifold, std::make_pair(y + 1, x+1));
    }
    else {
        pathAhead = exploreQuantumManifold(exploredPath, tachyonManifold, std::make_pair(y + 1, x));
    }
    exploredPath[pos] = pathAhead;
    return pathAhead;
}

unsigned long long ex2(char* inputfile) {
    std::ifstream istream(inputfile);
    std::string row;
    std::vector<std::string> tachyonManifold;
    path_map_t exploredPath;
    unsigned long long tachyonPosition;

    std::getline(istream, row);
    tachyonManifold.push_back(row);
    for (tachyonPosition = 0; row[tachyonPosition] != 'S'; tachyonPosition += 1);
    while (std::getline(istream, row))
        tachyonManifold.push_back(row);
    tachyon_posision_t startPos = std::make_pair(0, tachyonPosition);
    return exploreQuantumManifold(exploredPath, tachyonManifold, startPos);
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