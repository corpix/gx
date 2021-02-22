package main

import (
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"os"
)

const MiB_UNIT = 1 << 20

func checkError(err error) {
}
func uploadHandler(w http.ResponseWriter, r *http.Request) {
	r.ParseMultipartForm(32 * MiB_UNIT)
	file, handler, err := r.FormFile("upload")
	checkError(err)
	if err != nil {
		panic(err)
	}
	saveUpload(file, handler)
}
func saveUpload(file multipart.File, handler *multipart.FileHeader) {
	defer file.Close()
	fmt.Printf("Uploaded file info: %#v\n", handler.Header)
	localFilename := fmt.Sprintf("./uploads/%s", handler.Filename)
	f, err := os.OpenFile(localFilename, os.O_WRONLY|os.O_CREATE, 0666)
	if err != nil {
		panic(err)
	}
	defer f.Close()
	_, err = io.Copy(f, file)
	if err != nil {
		panic(err)
	}
}
func main() {
	http.HandleFunc("/", uploadHandler)
	addr := ":9090"
	fmt.Println("Listen on", addr)
	err := http.ListenAndServe(addr, nil)
	if err != nil {
		panic(err)
	}
}
