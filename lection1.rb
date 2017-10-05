class Person
    # static attribute
    @@count = 0

    # static getter
    def Person.counter
        @@count
    end

    # constructor
    def initialize (name, age)
        # @<name> - attribute
        # can be attached later
        # always private
        @name = name
        @age = age
    end

    #getter
    def name
        @name
    end

    #setter - special syntax
    protected 
        def age = (newAge)
            @age = newAge
        end
end

# derivative
class Monarch < Person
    def initialize (name, dynasty, age)
        super(name, age)
        @dynasty = dynasty
    end
end

# public - available to everybody
# private - method can be used only by object (in C++/Java - by class)
# protected - as usual
# all attributes - private only
# by default, methods are public

# dynamic access change

class Person
    public :age=;
end

# module usage #1

module History # cannot have instance
    class Person
        # ...
    end
end

# module usage #2

module SignificantEvents
    def addEvent (e)
        # ...
    end
end

class Person
    include SignificantEvents # as derivative
    # ...
end

