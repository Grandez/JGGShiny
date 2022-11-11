# Clase base para los paneles
JGGPanel = R6::R6Class("JGG.WEB.PANEL.INFO"
  ,cloneable  = FALSE
  ,portable   = TRUE
  ,lock_class = TRUE
  ,public = list(
     name       = NULL
    ,parent     = NULL  # Puntero al padre
    ,root       = NULL  # Puntero a raiz
    ,session    = NULL
    ,factory    = NULL
    ,data       = list()  # Datos
    ,vars       = list()  # Variables temporales con memoria
    ,cookies    = list()  # Variables con estado
    ,events     = list(listen = c(""), events=c(""))
    ,codes      = NULL
    ,parms      = NULL
    ,msg        = NULL
    ,inEvent    = FALSE  # Tag for nested events
    ,print      = function() { message(paste("Panel object for", self$name)) }
    ,initialize = function(id, parent, session) {
        # Force to create WEB Singleton when doesn't exist (teorically never)
        web = tryCatch({ WEB }, error = function(cond) { YATAWebEnv$new()})

        # Shortcuts to common objects
        self$name    = id
        self$parent  = parent
        self$session = session
        self$root    = private$getRoot()
        self$factory = web$factory
        self$cookies = web$getCookies(id)
        self$parms   = self$factory$parms
        self$msg     = self$factory$msg
    }
    ,getParent = function(name) {
        pp = self$parent
        while (!is.null(pp)) {
          if (pp$name == name) break
          pp = pp$parent
        }
        pp
    }
#    ,getCookie = function(key) { self$vars$cookies[[key]] }
    # ,setCookies = function() {
    #      WEB$setCookies(self$name, self$cookies)
    #     invisible(self)
    # }
    ,invalidate = function(panel) {
       if (!is.null(self$parent)) self$parent$invalidate(panel)
       private$loaded = FALSE
       invisible(self)
    }
#    ,isInvalid = function(panel) {
    ,invalid = function() {
        # Necesita recargar datos?
        if (!private$loaded) {
            private$loaded = TRUE
            return (TRUE)
        }
        if (!is.null(self$parent)) self$parent$invalid(panel)
     }
    ,reset     = function() {
        if (!is.null(self$parent)) self$parent$reset(self$name)
        invisible(self)
    }
    ,setCommarea       = function(...)            { WEB$setCommarea(...)        }
    ,getCommarea       = function(item=NULL, default=NULL) {
        WEB$getCommarea(item, default)
    }
    ,setCommareaBlock       = function(block, ...) {
        WEB$setCommareaBlock(block=block, ...)
     }
    ,getCommareaBlock       = function(block, item=NULL, default=NULL) {
        WEB$getCommareaBlock(block, item, default)
     }

  )
  ,private = list(
      loaded  = FALSE # Flag de carga
     ,getRoot = function() {
         tmp = self$parent
         while (!is.null(tmp)) tmp = tmp$parent
         tmp
      }


  )
)
