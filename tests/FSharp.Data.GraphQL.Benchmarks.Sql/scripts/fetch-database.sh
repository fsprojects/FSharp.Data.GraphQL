PROJECT_PATH="$( cd "$(dirname $0)"; cd ..; pwd -P)"
ZIP_FILE="${PROJECT_PATH}/ml-latest.zip"
DB_PATH="${PROJECT_PATH}/ml-latest"

echo "Checking if path \"${DB_PATH}\" exists..."

if [ ! -d "${DB_PATH}" ]
then
    echo "Path not found. Downloading database files..."
    wget -q -P "${PROJECT_PATH}" "http://files.grouplens.org/datasets/movielens/ml-latest.zip"
    echo "Download finished. Unzipping database files..."
    unzip "$ZIP_FILE"
    echo "Unzipping done. Removing zip file..."
    rm "$ZIP_FILE"
    echo "Database files successfuly downloaded."
else
    echo "Path already exists. Skipping download of database."
fi