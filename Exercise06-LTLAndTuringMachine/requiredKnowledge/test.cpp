#include <iostream>
#include <vector>
using namespace std;



vector<int> concat(vector<int> a, int v)
{
    a.push_back(v);
    return a;
}

void rec(int cur, int Max, vector<int> ans)
{
    if(cur > Max)
    {
        for(auto v : ans)
        {
            cout<<v<<" ";
        }
        cout<<endl;
        return;
    }

    rec(cur+1, Max, concat(ans, cur));

    rec(cur+1, Max, ans);
}

//1 2 3 4
//1 2 3





int main()
{
    vector<int> a;
    a.clear();
    rec(1, 10, a);

    return 0;
}