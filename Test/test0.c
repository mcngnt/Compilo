int a0;
int a1;
int a2;
int a3;
int a4;
int a5;
int a6;
int a7;
int a8;
int a9;
int a10;
int a11;
int a12;
int a13;
int a14;
int a15;
int a16;
int a17;
int a18;
int a19;
int a20;
int a21;
int a22;
int a23;
int a24;
int a31;
int a32;
int a33;
int a34;
int a35;
int a36;
int a37;
int a38;
int a39;
int a40;
int res;
int mem;

int main()
{
        int* p;
        int* start;
        int n;
        // a19 = 100;
        n = 12;
        start = "Test string to see if string length calculation works";
        start = &mem;
        p = &mem;
        *p = 0;
        ++p;
        *p = 1;
        while(p < start + n)
        {
                ++p;
                *p = *(p-1) + *(p-2);
        }
        res = *p;
        return 0;
}