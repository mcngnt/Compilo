
int x;

int main()
{
        int y;
        int z;
        int* p;
        p = NULL;
        x = -(0 - 5);
        {
                int w;
                w = 7;
        }
        y = x > 0 ? 3 : -100/2;
        z = (x * y) % 9;
        if(z < y)
        {
                while(z > y)
                {
                        --z;
                }
                y = 333;
        }
        else
        {
                p = &y;
                ++p;
                x = --(*p);
        }
        puts("-hello");
        return x;
}
