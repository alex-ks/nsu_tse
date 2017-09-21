# Реализуйте класс, аналогичный Array, с
# многопоточной реализацией итераторов: map,
# any?, all?, select. Объясните, можно ли таким
# образом реализовать итератор inject?
# Ограничения:
# - не допускается использовать циклы;
# - вся логика работы с потоками должны быть
# вынесена в отдельный метод, общий для всех
# итераторов.
# Количество потоков (и кусков) можно захардкодить из каких-то разумных соображений

require 'thread'

class ParallelArray
    @@threadCount = 4

private
    def doWork(mapFunc, localReduce, globalReduce)
        chunkSize = (@array.length / Float(@count)).ceil
        results = (0...@count).map {
            |i|
            Thread.new {
                start = i * chunkSize
                finish = if (i + 1) * chunkSize < @array.length then (i + 1) * chunkSize else @array.length end

                puts ["Chunk size:", finish - start, "Tid:", Thread.current.object_id].inspect

                localReduce.call ((start...finish).map { |j| mapFunc.call(@array[j]) })
            }
        }.map { |worker| worker.value }
        globalReduce.call(results)
    end

public
    def ParallelArray.threadCount
        @@threadCount
    end

    def ParallelArray.threadCount=(value)
        @@threadCount = value
    end

    def threadCount
        @count
    end

    def threadCount=(value)
        @count = value
    end

    def value
        @array
    end

    def inspect
        @array.inspect
    end

    def initialize(array)
        if not array.is_a?(Array)
            raise ArgumentError, "Array must be provided"
        end
        @array = array
        @count = @@threadCount
    end

    def map(&mapFunc)
        localReduce = lambda { |chunk| chunk }
        globalReduce = lambda { |chunks| ParallelArray.new(chunks.reduce([]) { |accum, elem| accum + elem }) }
        doWork(mapFunc, localReduce, globalReduce)
    end

    def select()
        mapFunc = lambda { |elem| [yield(elem), elem] }
        localReduce = lambda { |chunk| chunk.select { |tuple| tuple[0] }.map { |tuple| tuple[1] } }
        globalReduce = lambda { |chunks| ParallelArray.new(chunks.reduce([]) { |accum, elem| accum + elem }) }
        doWork(mapFunc, localReduce, globalReduce)
    end

    def any?(&cond)
        localReduce = lambda { |chunk| chunk.any? }
        globalReduce = lambda { |chunk| chunk.any? }
        doWork(cond, localReduce, globalReduce)
    end

    def all?(&cond)
        localReduce = lambda { |chunk| chunk.all? }
        globalReduce = lambda { |chunk| chunk.all? }
        doWork(cond, localReduce, globalReduce)
    end

end

ma = ParallelArray.new((1..30).to_a)
ParallelArray.threadCount = 2
puts ParallelArray.threadCount
puts ma.threadCount

puts ma.map {|i| i * 2 }.inspect
puts ma.select {|i| i % 2 == 0 }.inspect
puts ma.any? {|i| i % 2 == 0 }.inspect
puts ma.all? {|i| i % 2 == 0 }.inspect
puts ma.all? {|i| i % 1 == 0 }.inspect