#ifndef DRAMATIC_DRAMATIC_HPP
#define DRAMATIC_DRAMATIC_HPP

#include <cstddef>
#include <experimental/source_location>
#include <functional>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>

namespace drama
{

  namespace impl
  {

    template<typename... Elements>
    struct TypeList
    {
      template<typename TypeName>
      struct Element
      {
        using Value = TypeName;
      };

      template<std::size_t Index, typename = std::enable_if_t<(Index < sizeof...(Elements))>>
      auto constexpr Get() const noexcept
      {
        return std::get<Index>(Entries);
      }

      auto constexpr Empty() const noexcept -> bool
      {
        return !Size();
      }

      auto constexpr Size() const noexcept -> std::size_t
      {
        return sizeof...(Elements);
      }

    private:
      std::tuple<Element<Elements>...> Entries;
    };

    template<typename InvocableType, typename = void>
    struct Function
    {
    };

    template<typename InvocableType>
    struct Function<InvocableType, std::enable_if_t<!std::is_same_v<InvocableType, void>>>
    {
      template<typename... ArgumentTypes>
      auto constexpr operator()(ArgumentTypes &&... arguments) const -> std::invoke_result_t<InvocableType, ArgumentTypes...>
      {
        return Invokable(std::forward<ArgumentTypes>(arguments)...);
      }

      std::decay_t<InvocableType> Invokable;
    };

    template<typename ValueType>
    struct DoExpect
    {
      ValueType Value;
    };

    template<typename ValueType>
    struct DoesExpect
    {
      constexpr DoesExpect(DoExpect<ValueType> expect)
          : Value{std::move(expect.Value)}
      {
      }

      ValueType Value;
    };

    template<typename T>
    struct IsExpecting : std::false_type
    {
    };

    template<template<typename> typename WrapperType, typename WrappedType>
    struct IsExpecting<WrapperType<WrappedType>> : std::is_same<WrapperType<WrappedType>, DoesExpect<WrappedType>>
    {
    };

  }  // namespace impl

  enum struct PerformanceResult : bool
  {
    Failure,
    Success,
  };

  template<typename ExpectedType = void, typename InvocableType = void, typename... GivenTypes>
  struct Scene
  {
    using expected_type = std::conditional_t<std::is_same_v<ExpectedType, void>, std::monostate, ExpectedType>;
    using given_types = impl::TypeList<GivenTypes...>;

    explicit constexpr Scene(std::string_view name = {},
                             std::experimental::source_location location =
                                 std::experimental::source_location::current()) noexcept(std::is_nothrow_move_assignable_v<std::string_view>)
        : m_name{std::move(name)}
        , m_location{std::move(location)}
    {
    }

    auto constexpr Name() const & noexcept -> std::string_view const &
    {
      return m_name;
    }

    template<typename GivenType, typename EnablementBase = given_types, typename = std::enable_if_t<EnablementBase{}.Empty()>>
    auto constexpr Given(GivenType && given) && -> Scene<ExpectedType, InvocableType, GivenTypes..., GivenType>
    {
      return AppendGiven(std::forward<GivenType>(given));
    }

    template<typename GivenType, typename EnablementBase = given_types, typename = std::enable_if_t<!EnablementBase{}.Empty()>>
    auto constexpr AsWellAs(GivenType && given) && -> Scene<ExpectedType, InvocableType, GivenTypes..., GivenType>
    {
      return AppendGiven(std::forward<GivenType>(given));
    }

    template<typename NewInvocable,
             typename OldInvocableType = InvocableType,
             typename NewExpectedType = std::invoke_result_t<NewInvocable, GivenTypes...>,
             typename = std::enable_if_t<std::is_invocable_v<NewInvocable, GivenTypes...>>,
             typename = std::enable_if_t<std::is_same_v<OldInvocableType, void>>>
    auto constexpr Invoking(NewInvocable && invocable) && -> Scene<NewExpectedType, NewInvocable, GivenTypes...>
    {
      return Scene<NewExpectedType, NewInvocable, GivenTypes...>{
          std::move(*this),
          std::forward<NewInvocable>(invocable),
      };
    }

    template<typename YieldedType,
             typename CurrentExpectedType = ExpectedType,
             typename = std::enable_if_t<!impl::IsExpecting<CurrentExpectedType>::value>>
    auto constexpr Yields(YieldedType yielded) && -> Scene<impl::DoesExpect<ExpectedType>, InvocableType, GivenTypes...>
    {
      return Scene<impl::DoesExpect<ExpectedType>, InvocableType, GivenTypes...>{
          std::move(*this),
          impl::DoExpect<ExpectedType>{std::move(yielded)},
      };
    }

    auto constexpr Perform() const -> std::pair<PerformanceResult, std::experimental::source_location>
    {
      return std::pair{static_cast<PerformanceResult>(DoInvoke(std::index_sequence_for<GivenTypes...>{})), m_location};
    }

    explicit constexpr operator bool() const
    {
      return Perform().first == PerformanceResult::Success;
    }

  private:
    template<typename OtherExpectedType, typename OtherInvocableType, typename... OtherGivenTypes>
    friend struct Scene;

    template<typename NewGivenType, typename... OldGivenTypes, std::size_t... Indices>
    constexpr Scene(Scene<ExpectedType, InvocableType, OldGivenTypes...> old, NewGivenType && new_given, std::index_sequence<Indices...>)
        : m_name{std::move(old.m_name)}
        , m_location{std::move(old.m_location)}
        , m_givens{std::move(std::get<Indices>(old.m_givens))..., std::forward<NewGivenType>(new_given)}
    {
    }

    template<typename OldInvocable, typename OldExpectedType, typename NewInvocable>
    constexpr Scene(Scene<OldExpectedType, OldInvocable, GivenTypes...> old, NewInvocable && invocable)
        : m_name{std::move(old.m_name)}
        , m_location{std::move(old.m_location)}
        , m_givens{std::move(old.m_givens)}
        , m_invocable{std::forward<NewInvocable>(invocable)}
    {
    }

    template<typename OldExpectedType>
    constexpr Scene(Scene<OldExpectedType, InvocableType, GivenTypes...> old, impl::DoExpect<OldExpectedType> expected)
        : m_name{std::move(old.m_name)}
        , m_location{std::move(old.m_location)}
        , m_givens{std::move(old.m_givens)}
        , m_invocable{std::move(old.m_invocable)}
        , m_expected{std::move(expected)}
    {
    }

    template<typename GivenType>
    auto constexpr AppendGiven(GivenType && given) -> Scene<ExpectedType, InvocableType, GivenTypes..., GivenType>
    {
      return Scene<ExpectedType, InvocableType, GivenTypes..., GivenType>{
          std::move(*this),
          std::forward<GivenType>(given),
          std::index_sequence_for<GivenTypes...>{},
      };
    }

    template<std::size_t... Indices>
    auto constexpr DoInvoke(std::index_sequence<Indices...>) const -> bool
    {
      if constexpr (!std::is_same_v<expected_type, void>)
      {
        return std::invoke(m_invocable, std::get<Indices>(m_givens)...) == std::get<0>(m_expected).Value;
      }
      else
      {
        std::invoke(m_invocable, std::get<Indices>(m_givens)...);
        return true;
      }
    }

    std::string_view m_name{};
    std::experimental::source_location m_location{};
    std::tuple<GivenTypes...> m_givens{};
    impl::Function<InvocableType> m_invocable{};
    std::variant<expected_type> m_expected{};
  };

  template<typename NewGivenType, typename InvocableType, typename ExpectedType, typename... OldGivenTypes, std::size_t... Indices>
  Scene(Scene<ExpectedType, InvocableType, OldGivenTypes...>, NewGivenType &&, std::index_sequence<Indices...>)
      -> Scene<ExpectedType, InvocableType, OldGivenTypes..., NewGivenType>;

  template<typename OldExpectedType, typename OldInvocable, typename NewInvocable, typename... GivenTypes>
  Scene(Scene<OldExpectedType, OldInvocable, GivenTypes...>, NewInvocable &&)
      -> Scene<std::invoke_result_t<NewInvocable, GivenTypes...>, NewInvocable, GivenTypes...>;

  template<typename ExpectedType, typename InvocableType, typename... GivenTypes>
  Scene(Scene<ExpectedType, InvocableType, GivenTypes...>, impl::DoExpect<ExpectedType> expected)
      -> Scene<impl::DoesExpect<ExpectedType>, InvocableType, GivenTypes...>;

}  // namespace drama

#endif