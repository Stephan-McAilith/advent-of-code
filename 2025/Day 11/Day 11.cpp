#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <utility>
#include <algorithm>
#include <set>
#include <unordered_map>
#include <ranges>
#include <functional>
#include <iterator>
#include <numeric>

unsigned long long findPath(const std::unordered_map<std::string, std::vector<std::string>> &graph, const std::string &here, std::set<std::string> seen) {
    if (here == "out")
        return 1;
    if (seen.find(here) != seen.end())
        return 0;
    seen.insert(here);

    unsigned long long found = 0;
    for (const auto &output : graph.at(here))
        found += findPath(graph, output, seen);
    return found;
}



unsigned long long ex1(char *inputfile)
{
    std::ifstream istream(inputfile);
    std::string line;

    std::unordered_map<std::string, std::vector<std::string>> graph;
    while (std::getline(istream, line)) {
        std::stringstream lineStream(line);
        std::string device;
        std::string output;
        lineStream >> device;
        device = device.substr(0, device.size() - 1);
        while (lineStream >> output)
            graph[device].push_back(output);
    }
    return findPath(graph, "you", std::set<std::string>());
}



struct State {
    std::string here;
    bool dac;
    bool fft;
    bool operator==(const State&) const = default;
};

struct StateHash {
    std::size_t operator()(const State& k) const {
        return std::hash<std::string>{}(k.here)
            ^ std::hash<bool>{}(k.dac)
            ^ std::hash<bool>{}(k.fft);
    }
};

unsigned long long findPathWithRequirement(const std::unordered_map<std::string, std::vector<std::string>>& graph, State state, std::set<std::string> seen, std::unordered_map<State, unsigned long long, StateHash>&done) {
    if (state.here == "out") {
        if (state == State("out", true, true))
            return 1;
        else
            return 0;
    }
    if (state.here == "dac")
        state.dac = true;
    if (state.here == "fft")
        state.fft = true;

    if (done.find(state) != done.end())
        return done.at(state);
    if (seen.find(state.here) != seen.end())
        return 0;

    seen.insert(state.here);
    unsigned long long found = 0;
    for (const auto& output : graph.at(state.here)) {
        State nextState = state;
        nextState.here = output;
        found += findPathWithRequirement(graph, nextState, seen, done);
    }
    done.emplace(state, found);
    return found;
}


unsigned long long ex2(char* inputfile)
{
    std::ifstream istream(inputfile);
    std::string line;

    std::unordered_map<std::string, std::vector<std::string>> graph;
    while (std::getline(istream, line)) {
        std::stringstream lineStream(line);
        std::string device;
        std::string output;
        lineStream >> device;
        device = device.substr(0, device.size() - 1);
        while (lineStream >> output)
            graph[device].push_back(output);
    }
    std::unordered_map<State, unsigned long long, StateHash> done;
    
    return findPathWithRequirement(graph, State("svr", false, false), std::set<std::string>(), done);
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