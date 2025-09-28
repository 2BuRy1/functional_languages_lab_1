import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class First{


private static final int k = 13;
public static void main(String... args) {

        
        String s = new Scanner(System.in).nextLine();


        List<Integer> digitsList = new ArrayList<>();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (Character.isDigit(c)) {
                digitsList.add(c - '0');
            }
        }
        if (digitsList.size() < k || k <= 0) return;

        int n = digitsList.size();
        int maxProd = 0;

        for (int i = 0; i + k <= n; i++) {
            int prod = 1;
            for (int j = 0; j < k; j++) {
                prod *= digitsList.get(i + j);
            }
            if (prod > maxProd) maxProd = prod;
        }

        System.out.println(maxProd);
    }
}