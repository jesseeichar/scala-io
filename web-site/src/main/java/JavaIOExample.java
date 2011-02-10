import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URL;

public class JavaIOExample {
    public static void write(FileOutputStream out, String urlString) throws Exception {
        byte[] buffer = new byte[8192];
        URL url = new URL(urlString);
        InputStream in1 = url.openStream();
        try {
            int read = in1.read(buffer);
            while (read > -1) {
                out.write(buffer,0,read);
                read = in1.read(buffer);
            }
        } finally {
            in1.close();
        }

    }
    public static void main(String[] args) throws Exception {
        FileOutputStream out = new FileOutputStream("/tmp/javaout");
        try {
            write(out,"http://www.scala-lang.org");
            write(out,"http://www.scala-tools.org");
        } finally {
            out.close();
        }
    }
}
