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