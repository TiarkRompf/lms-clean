# LMS Code Walk

## Abstract

Lightweight Modular Staging (LMS) is a generative programming tool achieving the multi-stage programming (staging).
LMS_clean is a remake of the LMS project, aiming at a more flexible design and extension with better support for
LMS IR transformation and optimization. This documentation is a code walk of the LMS_clean repo, hoping to
explain the implementation (in high-level or in details) to people who are interested in learning and using LMS.
This is different from a tutorial since it will dive into the core implementation of LMS and offers more insights
than simply how to use LMS.

## Introduction

Multi-Stage Programming (or staging) is a programming language concept that allows various parts of the programs
to run in different stages. It allows users to code with high-level abstractions (trait, classes, high-order functions),
but still gain highly-efficient code after the abstractions are executed (staged away). This offers both high
productivity and high performance of the target program, thus the slogan "abstract without regret".

Lightweight Modular Staging (LMS) is a staging tool built in Scala. In LMS, type information is used to distinguish
the evaluation stages (i.e., All Rep[T] typed values and expressions are in code generation for the next stage.)
Simply speaking, LMS is a compiler. However, LMS does not have a laxer or parser to transform the input program
into intermediate representations (IR). Instead, the IR is generated via executing the input program. All the Rep[T]
typed expressions in the input program evaluate to LMS IR. This can be considered as the LMS frontend.
Then the LMS backend compiles the LMS IR to target programs.

[LMS Core](https://github.com/TiarkRompf/lms-clean/doc/main/scala/lms/core/backend.md)

[LMS Simple Frontend](https://github.com/TiarkRompf/lms-clean/doc/main/scala/lms/core/frontend.md)

[LMS Frontend](https://github.com/TiarkRompf/lms-clean/doc/main/scala/lms/core/stub.md)