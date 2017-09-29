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

class Threadpool
    def initialize(threadCount)
        @queue = Queue.new
        @working = true
        @threads = (1..threadCount).map { 
            |_| 
            Thread.new {
                while @working do
                    task = @queue.pop()
                    task.call()
                end
            }
        }.to_a
    end

    def enqueueTask(&task)
        @queue.push(task)
    end
end

class Task
    @@threadCount = 4
    @@threadPool = Threadpool.new @@threadCount

    def Task.threadCount 
        @@threadCount
    end

    def initialize (&task)
        @mutex = Mutex.new
        @condVar = ConditionVariable.new
        @finished = false
        @exception = nil

        @@threadPool.enqueueTask {
            begin
                @value = task.call()
            rescue Exception => e
                @exception = e
            end
            @mutex.synchronize {
                @finished = true
                @condVar.signal
            }
        }    
    end

    def value
        @mutex.synchronize {
            while not @finished do
                @condVar.wait(@mutex)
            end
        }
        if @exception != nil then raise @exception end
        @value
    end
end

def await (task)
    task.value
end

class Array
private
    def doWork(mapFunc, localReduce, globalReduce)
        mapResults = self.map { |elem| Task.new { mapFunc.call (elem) } }

        count = Task.threadCount
        chunkSize = (length / Float(Task.threadCount)).ceil

        results = mapResults.each_slice(chunkSize)
            .map { |slice| Task.new { localReduce.call (slice.map { |t| await t }) } }
            .map { |t| await t }

        globalReduce.call (results)
    end

public
    def Array.threadCount
        @@threadCount
    end

    def Array.threadCount=(value)
        @@threadCount = value
    end

    def map!(&mapFunc)
        localReduce = lambda { |chunk| chunk }
        globalReduce = lambda { |chunks| chunks.reduce([]) { |accum, elem| accum + elem } }
        doWork(mapFunc, localReduce, globalReduce)
    end

    def select!()
        mapFunc = lambda { |elem| [yield(elem), elem] }
        localReduce = lambda { |chunk| chunk.select { |tuple| tuple[0] }.map { |tuple| tuple[1] } }
        globalReduce = lambda { |chunks| chunks.reduce([]) { |accum, elem| accum + elem } }
        doWork(mapFunc, localReduce, globalReduce)
    end

    def any!(&cond)
        localReduce = lambda { |chunk| chunk.any? }
        globalReduce = lambda { |chunk| chunk.any? }
        doWork(cond, localReduce, globalReduce)
    end

    def all!(&cond)
        localReduce = lambda { |chunk| chunk.all? }
        globalReduce = lambda { |chunk| chunk.all? }
        doWork(cond, localReduce, globalReduce)
    end

end

ma = (1..30).to_a
mb = [0, 1, 2, 3, 5, 6, 7]

puts ma.map! { |i| i * 2 }.inspect
puts ma.select! { |i| i % 2 == 0 }.inspect
puts ma.any! { |i| i % 2 == 0 }.inspect
puts ma.all! { |i| i % 2 == 0 }.inspect
puts ma.all! { |i| i % 1 == 0 }.inspect
puts mb.map! { |i| i * 3 }.inspect
