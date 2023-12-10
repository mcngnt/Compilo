int res;

int print_int(int t)
{
    if(t <= 9)
    {
        putc('0' + t);
        return;
    }
    print_int(t/10);
    putc('0' + (t % 10));
    return;
}

int print_int_nl(int t)
{
    print_int(t);
    putc('\n');
    return;
}

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
        print_int_nl(res);
        return;
}