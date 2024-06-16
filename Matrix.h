//
// Created by 4s4r on 21.04.2024.
//

#ifndef TEMPLATEMATRIX_MATRIX_H
#define TEMPLATEMATRIX_MATRIX_H

#include <iostream>
#include <cstddef>
#include <fstream>
#include <vector>
#include <future>
#include <algorithm>

template<typename T>
class Matrix {
    T *data_;

public:
    size_t rows;
    size_t columns;

    explicit Matrix(size_t a = 2, size_t b = 2, bool identity = true, T data = 0) { // конструктор
        rows = a;
        columns = b;
        data_ = new T[rows * columns];
        if (identity && rows == columns) {
            for (size_t i = 0; i < rows * columns; ++i) {
                if (i % rows == i / rows) {
                    data_[i] = data;
                } else {
                    data_[i] = 0;
                }
            }
        } else {
            for (size_t i = 0; i < rows * columns; ++i) {
                data_[i] = 0;
            }
        }
    }

    ~Matrix() { //деструктор
        if (rows) delete[] data_;
        rows = 0;
        columns = 0;
    }

    Matrix(const Matrix &other) { // перегрузка оператора копирования
        rows = other.rows;
        columns = other.columns;
        data_ = new T[rows * columns];
        for (size_t i = 0; i < rows; i++) {
            for (size_t j = 0; j < columns; j++) {
                data_[i * columns + j] = other.get(i, j);
            }
        }
    }

    Matrix &operator=(const Matrix &other) { // перегрузка оператора присваивания
        if (this == &other) return *this;

        if (rows) delete[] data_;

        rows = other.rows;
        columns = other.columns;
        data_ = new T[rows * columns];
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                this->get(i, j) = other.get(i, j);
            }
        }
        return *this;
    }

    Matrix &operator*(T const scalar) { // перегрузка оператора * для умножения на скаляр
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                this->get(i, j) = get(i, j) * scalar;
            }
        }
        return *this;
    }

    Matrix operator*(const Matrix &other) const { // перегрузка умножения для матриц
        if (this->columns != other.rows) {
            std::cerr << "Uncorrect matrix sizes for \"*\"\n";
            Matrix matrix = Matrix(1, 1, false);
            return matrix;
        }

        Matrix result(rows, other.columns);
        for (size_t i = 0; i < this->rows; i++) {
            for (size_t j = 0; j < other.columns; j++) {
                T sum = 0;
                for (size_t k = 0; k < this->columns; k++) {
                    sum += this->get(i, k) * other.get(k, j);
                }
                result.get(i, j) = sum;
            }
        }
        return result;
    }

    Matrix operator+(const Matrix &other) const { // сложение матриц
        if (rows == other.rows && columns == other.columns) {
            Matrix result(rows, columns);
            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < columns; ++j) {
                    result.get(i, j) = this->get(i, j) + other.get(i, j);
                }
            }
            return result;
        } else {
            Matrix matrix = Matrix(1, 1, false);
            std::cerr << "Operator \"+\" with different matrix sizes\n";
            return matrix;
        }
    }

    Matrix operator-(const Matrix &other) const { // вычитание матриц
        if (rows == other.rows && columns == other.columns) {
            Matrix result(rows, columns);
            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < columns; ++j) {
                    result.get(i, j) = this->get(i, j) - other.get(i, j);
                }
            }
            return result;
        } else {
            Matrix matrix = Matrix(1, 1, false);
            std::cerr << "Operator \"-\" with different matrix sizes\n";
            return matrix;
        }
    }

    bool operator!=(const Matrix &other) const {  // перегрузка !=
        if (rows != other.rows || columns != other.columns) {
            return true;
        } else {
            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < columns; ++j) {
                    if (this->get(i, j) != other.get(i, j)) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    bool operator!=(const T &data) const {  // перегрузка != для сравнения со скаляром
        if (rows != columns) {
            return true;
        } else {
            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < columns; ++j) {
                    if (i == j) {
                        if (this->get(i, j) != data)
                            return true;
                    } else {
                        if (this->get(i, j) != 0) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }
    }

    bool operator==(const Matrix &other) const {  // перегрузка ==
        if (rows != other.rows || columns != other.columns) {
            return false;
        } else {
            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < columns; ++j) {
                    if (this->get(i, j) != other.get(i, j)) {
                        return false;
                    }
                }
            }
            return true;
        }
    }

    bool operator==(const T &data) const {  // перегрузка == для сравнения со скаляром
        if (rows != columns) {
            return false;
        } else {
            for (size_t i = 0; i < rows; ++i) {
                for (size_t j = 0; j < columns; ++j) {
                    if (i == j) {
                        if (this->get(i, j) != data)
                            return false;
                    } else {
                        if (this->get(i, j) != 0) {
                            return false;
                        }
                    }
                }
            }
            return true;
        }
    }

    Matrix operator!() {
        if (this->det() == 0) {
            std::cerr << "Try to find the inverse matrix with 0 determinant" << std::endl;
            return *this;
        }
        if (this->rows != this->columns) {
            std::cerr << "Try to find the inverse matrix for non-square matrix" << std::endl;
            return *this;
        }
        T inverse_det = 1 / (this->det());
        Matrix matrix_temp = this->mat_alg_add();
        matrix_temp = matrix_temp.transpose();
        matrix_temp = matrix_temp * inverse_det;
        return matrix_temp;
    }

    Matrix mat_alg_add() {
        if (this->rows != this->columns) {
            std::cerr << "Try to get matrix of algebraic additions from non-square matrix" << std::endl;
            return Matrix(1, 1);
        }
        Matrix result_matrix(this->rows, this->columns);
        for (size_t i = 0; i < this->rows; ++i) {
            for (size_t j = 0; j < this->columns; ++j) {
                Matrix temp_matrix(this->rows - 1, this->columns - 1);
                T temp_numb = 0;
                temp_matrix = this->minor(i, j);
                temp_numb = temp_matrix.det() * (((i % 2 + j % 2) % 2 == 0) ? 1 : -1);
                result_matrix.get(i, j) = temp_numb;
            }
        }
        return result_matrix;
    }

    Matrix transpose() {
        Matrix transpose_matrix(this->columns, this->rows);
        for (size_t i = 0; i < this->columns; ++i) {
            for (size_t j = 0; j < this->rows; ++j) {
                transpose_matrix.get(i, j) = this->get(j, i);
            }
        }
        return transpose_matrix;
    }

    void elem1(size_t a, size_t b) { // элементарное преобразование 1го типа
        if (a > rows || b > columns) {
            std::cerr << "Attempt at elementary type 1 conversion with invalid arguments" << std::endl;
        } else {
            for (size_t i = 0; i < columns; ++i) {
                T temp = 0;
                temp = this->get(a, i);
                this->get(a, i) = this->get(b, i);
                this->get(b, i) = temp;
            }
        }
    }

    void elem2(size_t row, T Numb) { // преобразование 2го типа
        for (size_t i = 0; i < columns; ++i) {
            this->get(row, i) = this->get(row, i) * Numb;
        }
    }

    void elem3(size_t to, size_t from, T Numb) { // преобразование 3го типа
        if (to > rows || from > rows) {
            std::cerr << "Attempt at elementary type 1 conversion with invalid arguments" << std::endl;
        } else {
            for (size_t i = 0; i < columns; ++i) {
                this->get(to, i) = this->get(to, i) + this->get(from, i) * Numb;
            }
        }
    }

    Matrix minor(size_t a, size_t b) {
        if (rows != columns) {
            std::cerr << "Try to get minor from non-square matrix" << std::endl;
            return *this; // Возвращаем текущий объект, который не изменился
        } else if (a >= rows || b >= columns) {
            std::cerr << "Invalid counts of rows or columns for getting a minor" << std::endl;
            return *this; // Возвращаем текущий объект, который не изменился
        }

        Matrix temp(rows - 1, columns - 1, false); // Создаем временную матрицу меньшего размера
        size_t d = 0, c;
        for (size_t i = 0; i < rows; ++i) {
            if (i == a) continue; // Пропускаем a-тую строку
            c = 0;
            for (size_t j = 0; j < columns; ++j) {
                if (j == b) continue; // Пропускаем b-тый столбец
                temp.get(d, c) = this->get(i, j);
                c++;
            }
            d++;
        }

        return temp;
    }

    T det() {
        if (this->rows == this->columns) {
            if (this->rows == 1)
                return this->get(0, 0);
            if (this->rows == 2)
                return this->get(0, 0) * this->get(1, 1) - this->get(0, 1) * this->get(1, 0);
            T result = 0;
            int sign = 0;
            for (size_t j = 0; j < this->columns; ++j) {
                Matrix temp(this->rows, this->columns);
                temp = this->minor(0, j);
                j % 2 == 0 ? sign = 1 : sign = -1;
                result += sign * this->get(0, j) * temp.det();
            }
            return result;
        } else {
            std::cerr << "Try to get determinant from non-square matrix" << std::endl;
            return 0;
        }
    }

    T &get(size_t i = 0, size_t j = 0) {
        if (i < rows && j < columns) {
            int index = i * columns + j;
            return data_[index];
        }
    }

    T &get(size_t i = 0, size_t j = 0) const { //чтобы возвр. значение не было отброшено
        if (i < rows && j < columns) {
            int index = i * columns + j;
            return data_[index];
        }
    }

    void show() const {
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                std::cout << this->get(i, j) << ' ';
            }
            std::cout << std::endl;
        }
    }

    void fill() {
        std::cout << "Enter " << columns << " real numbers in " << rows << " rows separated by \"space\": " << std::endl;
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < columns; ++j) {
                int index = i * columns + j;
                std::cin >> data_[index];
            }
        }
    }

    // Новые методы для параллельной обработки
    std::future<Matrix> parallel_add(const Matrix &other, size_t block_size) const {
        return std::async(std::launch::async, [this, &other, block_size]() {
            if (this->rows != other.rows || this->columns != other.columns) {
                std::cerr << "Uncorrect matrix sizes for parallel_add\n";
                return Matrix(1, 1, false);
            }

            Matrix result(this->rows, this->columns);
            std::vector<std::future<void>> futures;

            for (size_t i = 0; i < this->rows; i += block_size) {
                for (size_t j = 0; j < this->columns; j += block_size) {
                    futures.push_back(std::async(std::launch::async, [this, &other, &result, i, j, block_size]() {
                        size_t i_end = std::min(i + block_size, this->rows);
                        size_t j_end = std::min(j + block_size, this->columns);

                        for (size_t bi = i; bi < i_end; ++bi) {
                            for (size_t bj = j; bj < j_end; ++bj) {
                                result.get(bi, bj) = this->get(bi, bj) + other.get(bi, bj);
                            }
                        }
                    }));
                }
            }

            for (auto &f : futures) {
                f.get();
            }

            return result;
        });
    }

    std::future<Matrix> parallel_subtract(const Matrix &other, size_t block_size) const {
        return std::async(std::launch::async, [this, &other, block_size]() {
            if (this->rows != other.rows || this->columns != other.columns) {
                std::cerr << "Uncorrect matrix sizes for parallel_subtract\n";
                return Matrix(1, 1, false);
            }

            Matrix result(this->rows, this->columns);
            std::vector<std::future<void>> futures;

            for (size_t i = 0; i < this->rows; i += block_size) {
                for (size_t j = 0; j < this->columns; j += block_size) {
                    futures.push_back(std::async(std::launch::async, [this, &other, &result, i, j, block_size]() {
                        size_t i_end = std::min(i + block_size, this->rows);
                        size_t j_end = std::min(j + block_size, this->columns);

                        for (size_t bi = i; bi < i_end; ++bi) {
                            for (size_t bj = j; bj < j_end; ++bj) {
                                result.get(bi, bj) = this->get(bi, bj) - other.get(bi, bj);
                            }
                        }
                    }));
                }
            }

            for (auto &f : futures) {
                f.get();
            }

            return result;
        });
    }

    std::future<Matrix> parallel_multiply(const Matrix &other, size_t block_size) const {
        return std::async(std::launch::async, [this, &other, block_size]() {
            if (this->columns != other.rows) {
                std::cerr << "Uncorrect matrix sizes for parallel_multiply\n";
                return Matrix(1, 1, false);
            }

            Matrix result(this->rows, other.columns);
            std::vector<std::future<void>> futures;

            for (size_t i = 0; i < this->rows; i += block_size) {
                for (size_t j = 0; j < other.columns; j += block_size) {
                    futures.push_back(std::async(std::launch::async, [this, &other, &result, i, j, block_size]() {
                        size_t i_end = std::min(i + block_size, this->rows);
                        size_t j_end = std::min(j + block_size, other.columns);

                        for (size_t bi = i; bi < i_end; ++bi) {
                            for (size_t bj = j; bj < j_end; ++bj) {
                                T sum = 0;
                                for (size_t k = 0; k < this->columns; ++k) {
                                    sum += this->get(bi, k) * other.get(k, bj);
                                }
                                result.get(bi, bj) = sum;
                            }
                        }
                    }));
                }
            }

            for (auto &f : futures) {
                f.get();
            }

            return result;
        });
    }
};

template<typename T>
std::ostream &operator<<(std::ostream &out, const Matrix<T> &array) {
    out << "{";
    for (size_t i = 0; i < array.rows; i++) {
        out << "{";
        for (size_t j = 0; j < array.columns; j++) {
            out << array.get(i, j) << ',';
        }
        out << "}," << std::endl;
    }
    out << "}";

    return out;
}

template<typename T>
std::istream &operator>>(std::istream &in, Matrix<T> &array) {
    size_t row, col;
    in >> row >> col;
    array = Matrix(row, col);
    for (size_t i = 0; i < row; i++) {
        for (size_t j = 0; j < col; j++) {
            T value;
            in >> value;
            array.get(i, j) = value;
        }
    }
    return in;
}

#endif //TEMPLATEMATRIX_MATRIX_H
