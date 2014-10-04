int L, R, I, J;
int N = rand(1, 100000);
L = N / 2 + 1;
R = N;
if (L >= 2)
  {
    L = L - 1;
    assert (1 <= L && L <= N);
  }
else
  {
    assert (1 <= R && R <= N);
    assert (1 <= 1 && 1 <= N);
    R = R - 1;
  }
{
  int _1 = 0;
  while (R >= 2)
    
      {
        I = L;
        J = 2 * I;
        while (J <= R && rand(0, 1) == 0)
          {
            if (J <= R - 1)
              {
                assert (1 <= J && J <= N);
                assert (1 <= J + 1 && J + 1 <= N);
                if (rand(0, 1) == 0)
                  {
                    J = J + 1;
                  }
              }
            assert (1 <= J && J <= N);
            if (rand(0, 1) == 0)
              {
                assert (1 <= I && I <= N);
                assert (1 <= J && J <= N);
                I = J;
                J = 2 * J;
              }
          }
        assert (1 <= I && I <= N);
        if (L >= 2)
          {
            L = L - 1;
            assert (1 <= L && L <= N);
          }
        else
          {
            assert (1 <= R && R <= N);
            assert (1 <= 1 && 1 <= N);
            R = R - 1;
          }
        assert (1 <= 1 && 1 <= N);
      
      _1 = _1 + 1;
    }
}

