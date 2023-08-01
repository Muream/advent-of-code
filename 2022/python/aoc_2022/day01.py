from aocd.get import get_data
from aocd.post import submit


def main():
    day, year = (1, 2022)
    data = get_data(day=day, year=year)

    elves = sorted(map(lambda chunk: sum(map(int, chunk.split())), data.split("\n\n")))

    submit(elves[-1], part=1, day=day, year=year)
    submit(sum(elves[-3:]), part=2, day=day, year=year)


if __name__ == "__main__":
    main()
