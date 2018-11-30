#include <opencv2/opencv.hpp>
#include <iostream>
#include <vector>

bool drawing;
cv::Mat img;

typedef char byte;

union buffer{
    int integer;
    char chars[4];
};

void gmbARUMAITO(buffer &size){
    byte aux = size.chars[3];
    size.chars[3] = size.chars[0];
    size.chars[0] = aux;
    aux = size.chars[2];
    size.chars[2] = size.chars[1];
    size.chars[1] = aux;
}

void draw_circle(int event, int x, int y, int flags, void* param){
    if (event == cv::EVENT_LBUTTONDOWN){
        drawing = true;
    }else if(event == cv::EVENT_MOUSEMOVE){
        if (drawing == true)
            cv::circle(img,cv::Point(x,y),6,cv::Scalar(255,255,255),-1, cv::LINE_AA);
    }else if(event == cv::EVENT_LBUTTONUP){
        drawing = false;
        cv::circle(img,cv::Point(x,y),6,cv::Scalar(255,255,255),-1, cv::LINE_AA);
    }
}

void allData(std::string image_path, std::string label_path){
    std::ifstream image;
    std::ifstream label;
    image.open(image_path, std::ifstream::in);
    label.open(label_path, std::ifstream::in);
    buffer size;
    label.read(size.chars, 4);
    gmbARUMAITO(size);
    label.read(size.chars, 4);
    gmbARUMAITO(size);
    std::vector < byte > labels;
    byte l;
    for(register int i = 0; i < size.integer; i++){
        label.read(&l, 1);
        labels.push_back(l);
    }
    buffer rows, colunms;
    image.read(size.chars, 4);image.read(size.chars, 4);gmbARUMAITO(size);
    image.read(rows.chars, 4);gmbARUMAITO(rows);
    image.read(colunms.chars, 4);gmbARUMAITO(colunms);
    for(int a = 0; a < size.integer; a++){
        std::cout << ((int)labels[a]);
        for(register int i = 0; i < rows.integer; i++){
            for(register int j = 0; j < colunms.integer; j++){
                image.read(&l, 1);
                std::cout << " " << (((int)((unsigned char)l))/255.0);
            }
        }
        std::cout << std::endl;
    }
    return;
}

int main(int argc, char *argv[]){
    if(argc > 2){
        allData("", "");
        return 0;
    }
    drawing = false;
    img = cv::Mat(280, 280, CV_8UC3, cv::Scalar(0,0,0));
    cv::namedWindow("Desenhe");
    cv::setMouseCallback("Desenhe", draw_circle);
    while(1){
        cv::imshow("Desenhe",img);
        int k = cv::waitKey(1) & 0xFF;
        if (k == 's'){
            cv::Mat b;
            cv::Size size(28, 28);
            cv::resize(img, b, size);
            for(int i = 0; i < 28; i++){
                for(int j = 0; j < 28; j++){
                    std::cout <<  b.at<uchar>(cv::Point(i, j)) / 255.0 << " \n"[i == 27 && j == 27];
                }
            }
            cv::destroyAllWindows();
            break;
        }
    }
    return 0;
}