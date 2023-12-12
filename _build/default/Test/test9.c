int print_int(int t)
{
    if(t < 0)
    {
        putc('-');
        print_int(-t);
        return;
    }
    else if(t <= 9)
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
    puts("-3 % 10\n");
    print_int_nl(-3 % 10);
     puts("3 % -33\n");
    print_int_nl(3 % -33);
     puts("30 % 7\n");
    print_int_nl(30 % 7);
     puts("-11 % -10\n");
    print_int_nl(-11 % -10);

    puts("\n\n");

    puts("-3 / 1\n");
    print_int_nl(-3 / 1);
     puts("300 / -33\n");
    print_int_nl(300 / -33);
     puts("30 / 7\n");
    print_int_nl(30 / 7);
     puts("-11 / -10\n");
    print_int_nl(-11 / -10);
    return;
}