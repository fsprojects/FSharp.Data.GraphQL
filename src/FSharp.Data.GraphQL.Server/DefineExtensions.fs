namespace FSharp.Data.GraphQL.Server

open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL

/// Contains extensions for the Define module.
[<AutoOpen>]
module DefineExtensions =
    type Define with
        /// <summary>
        /// Defines an executor middleware by setting their sub-middleware functions.
        /// </summary>
        /// <param name="compile">The schema compile sub-middleware function.</param>
        /// <param name="postCompile">The schema post-compile sub-middleware function.</param>
        /// <param name="plan">The operation planning sub-middleware function.</param>
        /// <param name="execute">The operation execution sub-middleware function.</param>
        static member ExecutorMiddleware(?compile, ?postCompile, ?plan, ?execute) : IExecutorMiddleware =
            { new IExecutorMiddleware with 
                member __.CompileSchema = compile
                member __.PostCompileSchema = postCompile
                member __.PlanOperation = plan
                member __.ExecuteOperationAsync = execute }
