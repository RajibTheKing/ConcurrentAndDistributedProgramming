#include <bits/stdc++.h> 
using namespace std; 
using namespace std;
#define sz 5000100

int prime[1000000];
bool flag[sz+1000];
unsigned long long ans[5000100];

void SieveOfEratosthenes()
{
    int i, j, k=-1, r;
    for(i=3;i<=sz;i+=2)
        flag[i]=true;
    flag[2]=true;
    prime[++k]=2;


    for(i=3;i<sz;i+=2)
    {

        if(flag[i])
        {
            prime[++k]=i;
            if(sz/i>=i)
            {
                r=i*2;
                for(j=i*i;j<sz;j+=r)
                    flag[j]=false;
            }
        }
    }

    cout<<prime[k]<<" "<<k<<endl;
    for(int i=0; i<=10000; i++)
    {
        cout<<"prime: "<<prime[i]<<" "<<i<<endl;
    }

}

// Driver Program to test above function 
int main() 
{  
	SieveOfEratosthenes(); 
	return 0; 
} 
