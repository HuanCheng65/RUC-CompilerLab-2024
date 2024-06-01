#pragma once
#include <string>
#include <string_view>
#include <unordered_map>

using std::string, std::string_view;

class ReturnManager {
    struct ReturnInfo {
        int count;
        std::string label;
        std::string valName;
        bool endReturn;
    };

    std::unordered_map<std::string, ReturnInfo> returnInfos;

public:
    void newFunc(string_view funcName, string_view label, string_view valName)
    {
        returnInfos[funcName.data()] = { 0, string { label }, string { valName }, false };
    }

    void addReturn(string_view funcName)
    {
        returnInfos[funcName.data()].count++;
        returnInfos[funcName.data()].endReturn = true;
    }

    void setNotEndReturn(string_view funcName)
    {
        returnInfos[funcName.data()].endReturn = false;
    }

    bool isEndReturn(string_view funcName)
    {
        return returnInfos[funcName.data()].endReturn;
    }

    string getLabel(string_view funcName)
    {
        return returnInfos[funcName.data()].label;
    }

    string getValName(string_view funcName)
    {
        return returnInfos[funcName.data()].valName;
    }

    int getCount(string_view funcName)
    {
        return returnInfos[funcName.data()].count;
    }

    bool isSingleReturn(string_view funcName)
    {
        return returnInfos[funcName.data()].count == 1;
    }

    bool hasReturn(string_view funcName)
    {
        return returnInfos[funcName.data()].count > 0;
    }
};