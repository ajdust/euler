// trimming
// Trimming code thanks to https://stackoverflow.com/a/217605/4351294

#include <algorithm>
#include <cctype>
#include <locale>
#include <vector>

// trim from start (in place)
static inline void ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
        return !std::isspace(ch);
    }));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
    ltrim(s);
    rtrim(s);
}

// trim from start (copying)
static inline std::string ltrim_copy(std::string s) {
    ltrim(s);
    return s;
}

// trim from end (copying)
static inline std::string rtrim_copy(std::string s) {
    rtrim(s);
    return s;
}

// trim from both ends (copying)
static inline std::string trim_copy(std::string s) {
    trim(s);
    return s;
}

// trimming
// splitting

std::vector<std::string> split_space(std::string s)
{
    trim(s);

    std::vector<unsigned long> space_indices{};
    for (unsigned long i{ 0 }; i < s.length(); ++i) {
        if (s.at(i) == ' ') {
            space_indices.push_back(i);
        }
    }
    space_indices.push_back(s.length());

    std::vector<std::string> strings{};
    unsigned long start{ 0 };
    for (auto end : space_indices) {
        std::string sub = s.substr(start, end - start);
        if (sub != "")
            strings.push_back(sub);

        // set start to next character, and since end is a ' ', add one
        start = end + 1;
    }

    return strings;
}

// splitting
