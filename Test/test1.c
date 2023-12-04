int res;

int main()
{
        int* p;
        int* start;
        int n;
        n = 12;
        start = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
        p = start;
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