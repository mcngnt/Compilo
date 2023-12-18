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
    if(t < 0)
    {
        t = -t;
        putc('-');
    }
    print_int(t);
    putc('\n');
    return;
}

int main()
{
    int x;
    x = 10;
    {
        int x;
        x = 5;
    }
    print_int_nl(x);
    return;
}