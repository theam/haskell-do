{-
 - Copyright (c) 2017 The Agile Monkeys S.L. <hackers@theam.io>
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
module HaskellDo.GUI.External.SimpleMDE
  ( module HaskellDo.GUI.External.SimpleMDE.Common
#ifdef ghcjs_HOST_OS
  , module HaskellDo.GUI.External.SimpleMDE.JavascriptInternals
#else
  , module HaskellDo.GUI.External.SimpleMDE.ServerInternals
#endif
  )
where

#ifdef ghcjs_HOST_OS
import HaskellDo.GUI.External.SimpleMDE.JavascriptInternals
#else
import HaskellDo.GUI.External.SimpleMDE.ServerInternals
#endif

import HaskellDo.GUI.External.SimpleMDE.Common
