import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
public class Second{
public static void main(String...args){
    int limit = new Scanner(System.in).nextInt();
    if (limit <= 0) return ;

        List<Integer> abundants = new ArrayList<>();
        for (int n = 1; n <= limit; n++) {
            if (isAbundant(n)) abundants.add(n);
        }

        boolean[] can = new boolean[limit + 1];
        for (int i = 0; i < abundants.size(); i++) {
            int a = abundants.get(i);
            for (int j = i; j < abundants.size(); j++) { 
                int b = abundants.get(j);
                int sum = a + b;
                if (sum > limit) break;
                can[sum] = true;
            }
        }

        long total = 0;
        for (int n = 1; n <= limit; n++) {
            if (!can[n]) total += n;
        }
        System.out.println(total);
}


 public static boolean isAbundant(int n) {
        return sumProperDivisors(n) > n;
    }

    public static int sumProperDivisors(int n) {
        if (n <= 1) return 0;    
        int sum = 1;           
        int r = (int) Math.sqrt(n);

        for (int d = 2; d <= r; d++) {
            if (n % d == 0) {
                int q = n / d;
                sum += d;
                if (q != d) sum += q; 
            }
        }
        return sum;
    }
}