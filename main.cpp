#include <iostream>
#include <random>
#include <chrono>
#include <vector>
#include <thread>
#include "Matrix.h"

// Функция для заполнения матрицы случайными значениями
void fill_matrix_with_random_values(Matrix<int>& matrix, int min_value = 0, int max_value = 10) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(min_value, max_value);

    for (size_t i = 0; i < matrix.rows; ++i) {
        for (size_t j = 0; j < matrix.columns; ++j) {
            matrix.get(i, j) = dis(gen);
        }
    }
}

// Функция для измерения времени выполнения операции
template <typename Func>
auto measure_time(Func func) {
    auto start = std::chrono::high_resolution_clock::now();
    func();
    auto end = std::chrono::high_resolution_clock::now();
    return std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
}

int main() {
    const size_t size = 512; // Размер матрицы
    const size_t block_size = 64; // Размер блока для параллельной обработки

    // Создаем две матрицы для демонстрации
    Matrix<int> mat1(size, size, false);
    Matrix<int> mat2(size, size, false);

    // Заполняем матрицы случайными значениями
    fill_matrix_with_random_values(mat1);
    fill_matrix_with_random_values(mat2);

    std::cout << "Matrix 1:" << std::endl;
    // mat1.show();

    std::cout << "Matrix 2:" << std::endl;
    // mat2.show();

    // Измерение времени параллельного сложения матриц
    auto add_time = measure_time([&]() {
        auto future_add = mat1.parallel_add(mat2, block_size);
        future_add.get();
    });

    std::cout << "Time for parallel addition: " << add_time << " microseconds" << std::endl;

    // Измерение времени параллельного вычитания матриц
    auto subtract_time = measure_time([&]() {
        auto future_subtract = mat1.parallel_subtract(mat2, block_size);
        future_subtract.get();
    });

    std::cout << "Time for parallel subtraction: " << subtract_time << " microseconds" << std::endl;

    // Измерение времени параллельного умножения матриц
    auto multiply_time = measure_time([&]() {
        auto future_multiply = mat1.parallel_multiply(mat2, block_size);
        future_multiply.get();
    });

    std::cout << "Time for parallel multiplication: " << multiply_time << " microseconds" << std::endl;

    // Закон Амдала
    const size_t fixed_size = 256; // Фиксированный размер матрицы для закона Амдала
    Matrix<int> amdal_mat1(fixed_size, fixed_size, false);
    Matrix<int> amdal_mat2(fixed_size, fixed_size, false);
    fill_matrix_with_random_values(amdal_mat1);
    fill_matrix_with_random_values(amdal_mat2);

    // Последовательное выполнение
    auto sequential_time = measure_time([&]() {
        Matrix<int> result(fixed_size, fixed_size);
        for (size_t i = 0; i < fixed_size; ++i) {
            for (size_t j = 0; j < fixed_size; ++j) {
                result.get(i, j) = 0;
                for (size_t k = 0; k < fixed_size; ++k) {
                    result.get(i, j) += amdal_mat1.get(i, k) * amdal_mat2.get(k, j);
                }
            }
        }
    });

    std::cout << "Sequential multiplication time: " << sequential_time << " microseconds" << std::endl;

    // Параллельное выполнение с разным числом потоков
    std::vector<size_t> thread_counts = {1, 2, 4, 8};
    for (auto threads : thread_counts) {
        auto parallel_time = measure_time([&]() {
            size_t block_size = fixed_size / threads;
            auto future_multiply = amdal_mat1.parallel_multiply(amdal_mat2, block_size);
            future_multiply.get();
        });

        double empirical_speedup = static_cast<double>(sequential_time) / parallel_time;

        std::cout << "Threads: " << threads << std::endl;
        std::cout << "Parallel multiplication time: " << parallel_time << " microseconds" << std::endl;
        std::cout << "Empirical speedup: " << empirical_speedup << std::endl;
        std::cout << "-----------" << std::endl;
    }

    return 0;
}
