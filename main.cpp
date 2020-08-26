#include "dramatic/dramatic.hpp"

#include <iostream>

auto main() -> int
{
  // clang-format off
  auto constexpr first_scene = drama::Scene{"1 + 1 equals 2"}
                                        .Given(1)
                                        .AsWellAs(1)
                                        .Invoking(std::plus<>{})
                                        .Yields(4);

  auto result = first_scene.Perform();

  std::cout << '\'' << first_scene.Name() << "' "
            << "returned '" << std::boolalpha << (result.first == drama::PerformanceResult::Success)
            << "' @ " << result.second.file_name() << ':' << result.second.line() << '\n';
  // clang-format on

  static_assert(drama::Scene{}.Given(2).AsWellAs(1).Invoking(std::plus<>{}).Yields(3));
}