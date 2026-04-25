## Algorytm N-R

1. Algorytm Newtona Raphsona może nie być zbieżny
2. Jeśli algorytm jest zbieżny, to do najbliższego punktu stacjonarnego, niekoniecznie do maksimum globalnego.
3. Warto uruchomić algorytm kilka razy, od różnych punktów początkwoych

Jeśli Hesjan jest ujemnie określona, to wtedy w punkcie w którym gradient się zeruje jest maksimum globalne. Ale zazwyaczaj hesjan nie spełnia tego warunku

## Asymptotyczna macierz kowariancji

$\widehat{\beta}$ - estymator największej wiarygodności wektora $\beta$, wyznaczony za pomocą algorytmu N-R. Z własności asymptotycznych estymatów NW wynika, że $$\widehat{\beta} - \beta \sim^d\sim = \mathcal{N}(\bold{0}, \Sigma)$$
gdzie $\Sigma = I^{-1}(\beta)$

$I(\beta)$ - macierz informacji Fishera skonstruowana na podstawie próby $(y_1,\bold{x}_1),...,(y_n,\bold{x}_n)$.

$$ $$

