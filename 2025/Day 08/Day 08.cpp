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


double distance(const std::tuple<int, int, int>& box1, const std::tuple<int, int, int>& box2) {
    return sqrt(pow(std::get<0>(box1) - std::get<0>(box2), 2) + pow(std::get<1>(box1) - std::get<1>(box2), 2) + pow(std::get<2>(box1) - std::get<2>(box2), 2));
}

unsigned long long ex1(char* inputfile) {
	std::ifstream istream(inputfile);
    std::string line;
    std::vector<std::tuple<int, int, int>> junctionBoxes;
    std::vector<std::pair<double, std::pair<std::vector<std::tuple<int, int, int>>::iterator, std::vector<std::tuple<int, int, int>>::iterator>>> junctionBoxLinks;

    while (std::getline(istream, line)) {
        int x, y, z;
        char comma;
        std::istringstream lineStream(line);
        lineStream >> x >> comma >> y >> comma >> z;
        junctionBoxes.push_back(std::make_tuple(x, y, z));
    }
    for (auto it = junctionBoxes.begin(); it != junctionBoxes.end(); it++) {
        for (auto itBis = it; ++itBis != junctionBoxes.end();) {
            junctionBoxLinks.push_back(std::make_pair(distance(*it,*itBis), std::make_pair(it, itBis)));
        }
    }
    std::sort(junctionBoxLinks.begin(), junctionBoxLinks.end(), [](auto const& a, auto const& b) {return a.first < b.first; });
    std::vector<std::set<std::vector<std::tuple<int, int, int>>::iterator>> circuits;
    for (int i = 0; i < 1000; i += 1) {
        auto box1 = junctionBoxLinks[i].second.first, box2 = junctionBoxLinks[i].second.second;
        auto box1Circuit = circuits.end(), box2Circuit = circuits.end();
        for (auto it = circuits.begin(); it != circuits.end(); it++) {
            if (it->find(box1) != it->end()) box1Circuit = it;
            if (it->find(box2) != it->end()) box2Circuit = it;
        }
        if (box1Circuit == circuits.end()) {
            if (box2Circuit == circuits.end())
                circuits.push_back(std::set({ box1, box2 }));
            else
                box2Circuit->insert(box1);
        }
        else {
            if (box2Circuit == circuits.end())
                box1Circuit->insert(box2);
            else if (box1Circuit != box2Circuit) {
                box1Circuit->merge(*box2Circuit);
                circuits.erase(box2Circuit);
            }
        }
    }
    std::sort(circuits.begin(), circuits.end(), [](auto const& a, auto const& b) {return a.size() > b.size();});
    unsigned long long finalProduct = 1;
    for (int i = 0; i < 3 && i < circuits.size(); i += 1)
        finalProduct *= circuits[i].size();
    return finalProduct;
}

unsigned long long ex2(char* inputfile) {
    std::ifstream istream(inputfile);
    std::string line;
    std::vector<std::tuple<int, int, int>> junctionBoxes;
    std::vector<std::pair<double, std::pair<std::vector<std::tuple<int, int, int>>::iterator, std::vector<std::tuple<int, int, int>>::iterator>>> junctionBoxLinks;

    while (std::getline(istream, line)) {
        int x, y, z;
        char comma;
        std::istringstream lineStream(line);
        lineStream >> x >> comma >> y >> comma >> z;
        junctionBoxes.push_back(std::make_tuple(x, y, z));
    }
    for (auto it = junctionBoxes.begin(); it != junctionBoxes.end(); it++) {
        for (auto itBis = it; ++itBis != junctionBoxes.end();) {
            junctionBoxLinks.push_back(std::make_pair(distance(*it, *itBis), std::make_pair(it, itBis)));
        }
    }
    std::sort(junctionBoxLinks.begin(), junctionBoxLinks.end(), [](auto const& a, auto const& b) {return a.first < b.first; });
    int i = 0;
    std::vector<std::tuple<int, int, int>>::iterator box1, box2;
    std::vector<std::set<std::vector<std::tuple<int, int, int>>::iterator>> circuits;
    while (!(circuits.size() == 1 && circuits.begin()->size() == junctionBoxes.size())) {
        box1 = junctionBoxLinks[i].second.first;
        box2 = junctionBoxLinks[i].second.second;
        auto box1Circuit = circuits.end(), box2Circuit = circuits.end();
        for (auto it = circuits.begin(); it != circuits.end(); it++) {
            if (it->find(box1) != it->end()) box1Circuit = it;
            if (it->find(box2) != it->end()) box2Circuit = it;
        }
        if (box1Circuit == circuits.end()) {
            if (box2Circuit == circuits.end())
                circuits.push_back(std::set({ box1, box2 }));
            else
                box2Circuit->insert(box1);
        }
        else {
            if (box2Circuit == circuits.end())
                box1Circuit->insert(box2);
            else if (box1Circuit != box2Circuit) {
                box1Circuit->merge(*box2Circuit);
                circuits.erase(box2Circuit);
            }
        }
        i += 1;
    }
    return std::get<0>(*box1) * std::get<0>(*box2);
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