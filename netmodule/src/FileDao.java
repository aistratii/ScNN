import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Created by aistratii on 06-07-2016.
 */
public class FileDao {
    private String fileName;
    private FileInputStream in = null;

    FileDao(String fileName) {
        this.fileName = fileName;
        try {
            in = new FileInputStream(fileName);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public byte[] nextChunk(int maxSizeChunk) {
        byte buff[] = new byte[maxSizeChunk];
        try {
            in.read(buff);
        } catch (IOException e) {
            e.printStackTrace();
        }


        return buff;
    }
}
