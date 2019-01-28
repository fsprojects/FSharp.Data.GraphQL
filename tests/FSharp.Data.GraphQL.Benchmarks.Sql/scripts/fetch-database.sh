DB_PATH = "../ml-latest"
echo "Checking if path \"$DB_PATH\" exists..."
if [ ! -d "$DB_PATH" ]
then
    echo "Path not found. Downloading database files..."
    wget -P "$DB_PATH" "http://files.grouplens.org/datasets/movielens/ml-latest.zip"
    echo "Download finished. Unzipping database files..."
    ZIP_FILE = "$DB_PATH/ml-latest.zip"
    unzip "$ZIP_FILE"
    echo "Unzipping done. Removing zip file..."
    rm "$ZIP_FILE"
    echo "Database files successfuly downloaded."
else
    echo "Path already exists. Skipping download of database."
fi