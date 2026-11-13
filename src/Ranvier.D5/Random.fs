namespace Ranvier.D5

open System

/// <summary>An immutable xoshiro256** generator state. Advancing returns a new state; earlier states stay valid, so any draw can be replayed.</summary>
/// <remarks>Ranvier.Anime's seeded random is a stateful closure. Charts and simulations need explicit state they can store, branch and replay, so D5 owns this small generator. It is not cryptographically secure.</remarks>
[<Struct>]
type RandomState = private { S0:uint64; S1:uint64; S2:uint64; S3:uint64 }

/// <summary>Seeding, advancing and splitting explicit random states.</summary>
[<RequireQualifiedAccess>]
module RandomState =
    let private splitMix (value:uint64) =
        let next=value+0x9E3779B97F4A7C15UL
        let mutable z=next
        z<-(z^^^(z>>>30))*0xBF58476D1CE4E5B9UL
        z<-(z^^^(z>>>27))*0x94D049BB133111EBUL
        struct(z^^^(z>>>31),next)

    /// <summary>Expands a 64-bit seed with splitmix64, as the xoshiro authors recommend; equal seeds give equal streams.</summary>
    let create (seed:uint64) =
        let struct(a,s1)=splitMix seed
        let struct(b,s2)=splitMix s1
        let struct(c,s3)=splitMix s2
        let struct(d,_)=splitMix s3
        {S0=a;S1=b;S2=c;S3=(if a|||b|||c|||d=0UL then 1UL else d)}

    let private rotate (value:uint64) count=(value<<<count)|||(value>>>(64-count))

    /// <summary>Returns 64 random bits and the following state.</summary>
    let nextUInt64 state =
        let result=rotate (state.S1*5UL) 7*9UL
        let t=state.S1<<<17
        let s2=state.S2^^^state.S0
        let s3=state.S3^^^state.S1
        let s1=state.S1^^^s2
        let s0=state.S0^^^s3
        struct(result,{S0=s0;S1=s1;S2=s2^^^t;S3=rotate s3 45})

    /// <summary>Returns a uniform float in [0,1) with 53 random bits and the following state.</summary>
    let nextFloat state =
        let struct(bits,next)=nextUInt64 state
        struct(float(bits>>>11)*(1./9007199254740992.),next)

    /// <summary>Derives an independent child stream (seeded from fresh bits) and the parent's following state.</summary>
    let split state =
        let struct(bits,next)=nextUInt64 state
        struct(create bits,next)

/// <summary>A validated probability distribution; construct it through the Distribution module.</summary>
[<RequireQualifiedAccess>]
type Distribution =
    private
    | Uniform of low:float * high:float
    | Integer of low:float * high:float
    | Normal of mean:float * deviation:float
    | LogNormal of mean:float * deviation:float
    | Exponential of rate:float
    | Gamma of shape:float * scale:float
    | Beta of alpha:float * beta:float
    | Poisson of mean:float
    | Binomial of trials:int * probability:float
    | Geometric of probability:float
    | Bernoulli of probability:float
    | Pareto of alpha:float
    | Logistic of location:float * scale:float
    | Weibull of shape:float * location:float * scale:float
    | Cauchy of location:float * scale:float
    | IrwinHall of count:int
    | Bates of count:int

/// <summary>Validated distributions following d3-random, sampled from explicit immutable states.</summary>
/// <remarks>Invalid parameters are rejected when a distribution is created, so sampling is total. Every sample consumes a deterministic number of draws except the rejection samplers (gamma, beta, Poisson), which stay deterministic for a given state.</remarks>
[<RequireQualifiedAccess>]
module Distribution =
    let private finite values=values |> List.forall Double.IsFinite
    let private check valid message distribution=if valid then Ok distribution else Error [InvalidObservation message]

    /// <summary>Uniform on [low,high).</summary>
    let uniform low high = check (finite [low;high;high-low] && low<=high) "Uniform bounds must be finite with low <= high." (Distribution.Uniform(low,high))
    /// <summary>Integers in [low,high), as floor of a uniform draw.</summary>
    let integer low high = check (finite [low;high;high-low] && low<high) "Integer bounds must be finite with low < high." (Distribution.Integer(floor low,floor high))
    /// <summary>Gaussian by the Box–Muller transform, using two draws per sample and no cached spare.</summary>
    let normal mean deviation = check (finite [mean;deviation] && deviation>=0.) "Normal parameters must be finite with nonnegative deviation." (Distribution.Normal(mean,deviation))
    /// <summary>exp of a normal draw with the given underlying mean and deviation.</summary>
    let logNormal mean deviation = check (finite [mean;deviation] && deviation>=0.) "Log-normal parameters must be finite with nonnegative deviation." (Distribution.LogNormal(mean,deviation))
    /// <summary>Exponential with positive rate (mean 1/rate).</summary>
    let exponential rate = check (Double.IsFinite rate && rate>0.) "Exponential rate must be positive and finite." (Distribution.Exponential rate)
    /// <summary>Gamma with shape k and scale theta (Marsaglia–Tsang).</summary>
    let gamma shape scale = check (finite [shape;scale] && shape>0. && scale>0.) "Gamma shape and scale must be positive and finite." (Distribution.Gamma(shape,scale))
    /// <summary>Beta on [0,1] from two gamma draws.</summary>
    let beta alpha beta = check (finite [alpha;beta] && alpha>0. && beta>0.) "Beta parameters must be positive and finite." (Distribution.Beta(alpha,beta))
    /// <summary>Poisson counts: multiplication below mean 10, Hörmann's transformed rejection (PTRS) above.</summary>
    let poisson mean = check (Double.IsFinite mean && mean>=0. && mean<=1e12) "Poisson mean must be finite in [0,1e12]." (Distribution.Poisson mean)
    /// <summary>Binomial counts by d3-random's beta splitting followed by geometric counting.</summary>
    let binomial trials probability = check (trials>=0 && Double.IsFinite probability && probability>=0. && probability<=1.) "Binomial needs nonnegative trials and probability in [0,1]." (Distribution.Binomial(trials,probability))
    /// <summary>Trials until the first success, at least 1.</summary>
    let geometric probability = check (Double.IsFinite probability && probability>0. && probability<=1.) "Geometric probability must be in (0,1]." (Distribution.Geometric probability)
    /// <summary>1 with the given probability, otherwise 0.</summary>
    let bernoulli probability = check (Double.IsFinite probability && probability>=0. && probability<=1.) "Bernoulli probability must be in [0,1]." (Distribution.Bernoulli probability)
    /// <summary>Pareto with minimum 1 and positive shape alpha.</summary>
    let pareto alpha = check (Double.IsFinite alpha && alpha>0.) "Pareto alpha must be positive and finite." (Distribution.Pareto alpha)
    /// <summary>Logistic with location and positive scale.</summary>
    let logistic location scale = check (finite [location;scale] && scale>0.) "Logistic scale must be positive and finite." (Distribution.Logistic(location,scale))
    /// <summary>Weibull with shape k, location and scale; k = 0 gives the Gumbel distribution, as in d3-random.</summary>
    let weibull shape location scale = check (finite [shape;location;scale] && scale>0.) "Weibull parameters must be finite with positive scale." (Distribution.Weibull(shape,location,scale))
    /// <summary>Cauchy with location and positive scale.</summary>
    let cauchy location scale = check (finite [location;scale] && scale>0.) "Cauchy scale must be positive and finite." (Distribution.Cauchy(location,scale))
    /// <summary>Sum of count uniform draws.</summary>
    let irwinHall count = check (count>=0 && count<=1_000_000) "Irwin–Hall count must be in [0,1000000]." (Distribution.IrwinHall count)
    /// <summary>Mean of count uniform draws.</summary>
    let bates count = check (count>=1 && count<=1_000_000) "Bates count must be in [1,1000000]." (Distribution.Bates count)

    let private uniformOpen state =
        let struct(value,next)=RandomState.nextFloat state
        struct(1.-value,next)

    let private standardNormal state =
        let struct(u,a)=uniformOpen state
        let struct(v,b)=RandomState.nextFloat a
        struct(sqrt(-2.*log u)*cos(2.*Math.PI*v),b)

    let rec private standardGamma shape state =
        if shape<1. then
            let struct(value,next)=standardGamma (shape+1.) state
            let struct(u,last)=uniformOpen next
            struct(value*Math.Pow(u,1./shape),last)
        else
            let d=shape-1./3.
            let c=1./sqrt(9.*d)
            let mutable current=state
            let mutable result=nan
            while Double.IsNaN result do
                let struct(x,a)=standardNormal current
                let v=1.+c*x
                if v<=0. then current<-a
                else
                    let v=v*v*v
                    let struct(u,b)=uniformOpen a
                    current<-b
                    if log u<0.5*x*x+d-d*v+d*log v then result<-d*v
            struct(result,current)

    // Lanczos approximation (g = 7, n = 9) of log Gamma for the Poisson acceptance test. The table is shared: built per
    // call, it is a heap allocation on every rejection step until the optimising JIT stack-allocates it.
    let private lanczos=[|0.99999999999980993;676.5203681218851;-1259.1392167224028;771.32342877765313;-176.61502916214059;
                          12.507343278686905;-0.13857109526572012;9.9843695780195716e-6;1.5056327351493116e-7|]
    let private logGamma (x:float) =
        let coefficients=lanczos
        let x=x-1.
        let mutable sum=coefficients[0]
        for index=1 to 8 do sum<-sum+coefficients[index]/(x+float index)
        let t=x+7.5
        0.5*log(2.*Math.PI)+(x+0.5)*log t-t+log sum

    let private poissonDraw mean state =
        if mean=0. then struct(0.,state)
        elif mean<10. then
            let limit=exp(-mean)
            let mutable product=1.
            let mutable count= -1.
            let mutable current=state
            while product>limit do
                let struct(u,next)=RandomState.nextFloat current
                product<-product*u
                count<-count+1.
                current<-next
            struct(count,current)
        else
            let root=sqrt mean
            let logMean=log mean
            let b=0.931+2.53*root
            let a= -0.059+0.02483*b
            let inverseAlpha=1.1239+1.1328/(b-3.4)
            let ratio=0.9277-3.6224/(b-2.)
            let mutable current=state
            let mutable result=nan
            while Double.IsNaN result do
                let struct(u,first)=RandomState.nextFloat current
                let struct(v,second)=RandomState.nextFloat first
                current<-second
                let u=u-0.5
                let us=0.5-abs u
                let k=floor((2.*a/us+b)*u+mean+0.43)
                if us>=0.07 && v<=ratio then result<-k
                elif k>=0. && not(us<0.013 && v>us) &&
                     log v+log inverseAlpha-log(a/(us*us)+b)<= -mean+k*logMean-logGamma(k+1.) then result<-k
            struct(result,current)

    let private geometricDraw probability state =
        if probability=1. then struct(1.,state)
        else
            let struct(u,next)=RandomState.nextFloat state
            struct(floor(Math.Log(1.-u)/Math.Log(1.-probability))+1.,next)

    let rec private draw distribution state =
        match distribution with
        | Distribution.Uniform(low,high) ->
            let struct(u,next)=RandomState.nextFloat state
            struct(low+(high-low)*u,next)
        | Distribution.Integer(low,high) ->
            let struct(u,next)=RandomState.nextFloat state
            struct(min (high-1.) (floor(low+(high-low)*u)),next)
        | Distribution.Normal(mean,deviation) ->
            let struct(z,next)=standardNormal state
            struct(mean+deviation*z,next)
        | Distribution.LogNormal(mean,deviation) ->
            let struct(z,next)=standardNormal state
            struct(exp(mean+deviation*z),next)
        | Distribution.Exponential rate ->
            let struct(u,next)=uniformOpen state
            struct(-log u/rate,next)
        | Distribution.Gamma(shape,scale) ->
            let struct(value,next)=standardGamma shape state
            struct(value*scale,next)
        | Distribution.Beta(alpha,beta) ->
            let struct(x,a)=standardGamma alpha state
            let struct(y,b)=standardGamma beta a
            struct((if x+y=0. then 0.5 else x/(x+y)),b)
        | Distribution.Poisson mean -> poissonDraw mean state
        | Distribution.Binomial(trials,probability) ->
            let mutable accumulated=0.
            let mutable remaining=float trials
            let mutable p=probability
            let mutable current=state
            while remaining*p>16. && remaining*(1.-p)>16. do
                let index=floor((remaining+1.)*p)
                let struct(y,next)=draw (Distribution.Beta(index,remaining-index+1.)) current
                current<-next
                if y<=p then
                    accumulated<-accumulated+index
                    remaining<-remaining-index
                    p<-(p-y)/(1.-y)
                else
                    remaining<-index-1.
                    p<-p/y
            let low=p<0.5
            let q=if low then p else 1.-p
            let mutable count=0.
            if q>0. then
                let struct(first,next)=geometricDraw q current
                let mutable sum=first
                current<-next
                while sum<=remaining do
                    let struct(step,following)=geometricDraw q current
                    sum<-sum+step
                    count<-count+1.
                    current<-following
            struct(accumulated+(if low then count else remaining-count),current)
        | Distribution.Geometric probability -> geometricDraw probability state
        | Distribution.Bernoulli probability ->
            let struct(u,next)=RandomState.nextFloat state
            struct((if u<probability then 1. else 0.),next)
        | Distribution.Pareto alpha ->
            let struct(u,next)=uniformOpen state
            struct(1./Math.Pow(u,1./alpha),next)
        | Distribution.Logistic(location,scale) ->
            let struct(u,next)=uniformOpen state
            struct(location+scale*log(u/(1.-u)),next)
        | Distribution.Weibull(shape,location,scale) ->
            let struct(u,next)=uniformOpen state
            let e= -log u
            struct(location+scale*(if shape=0. then -log e else Math.Pow(e,1./shape)),next)
        | Distribution.Cauchy(location,scale) ->
            let struct(u,next)=RandomState.nextFloat state
            struct(location+scale*tan(Math.PI*(u-0.5)),next)
        | Distribution.IrwinHall count ->
            let mutable sum=0.
            let mutable current=state
            for _ in 1..count do
                let struct(u,next)=RandomState.nextFloat current
                sum<-sum+u
                current<-next
            struct(sum,current)
        | Distribution.Bates count ->
            let struct(sum,next)=draw (Distribution.IrwinHall count) state
            struct(sum/float count,next)

    /// <summary>Draws one value and returns the following state.</summary>
    let sample distribution state = draw distribution state

    /// <summary>Draws a bounded number of values in order, returning them with the final state.</summary>
    let samples count distribution state : Result<float array*RandomState,LayoutError list> =
        if count<0 || count>100_000_000 then Error [BudgetExceeded "Sample count must be in [0,100000000]."]
        else
            let values=Array.zeroCreate<float> count
            let mutable current=state
            for index=0 to count-1 do
                let struct(value,next)=draw distribution current
                values[index]<-value
                current<-next
            Ok(values,current)
