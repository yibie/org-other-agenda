# 合成议程夹具

`agenda.org` 是 org-other-agenda 的测试夹具。它不是真实数据，而是为覆盖
本包需要处理的各种 agenda 语义而构造的。

## 固定基准日

夹具中的日期全部围绕 **2026-08-20（周四）** 构造，所在周为
2026-08-17 Mon .. 2026-08-23 Sun。

**测试必须显式传入 START-DAY**，不要依赖真实的 today：

```elisp
(org-agenda-run-series
 "org-other-agenda"
 '(((agenda "" ((org-agenda-start-day "2026-08-20")
                (org-agenda-span 'week)))
    (alltodo ""))))
```

**但只钉 START-DAY 不够。** `org-agenda-start-day` 只固定周视图的起始日，
固定不了「今天」：逾期重复条目（`Sched. Nx`）、`now` 分隔线、`N d. ago`
这类相对表述全部跟随真实 today。实测证据：同一份夹具在 2026-08-20 与
2026-08-21 两天运行，逾期条目从 3 项变为 9 项，且归属日期整体后移一天。

测试必须同时钉死「今天」：

```elisp
(let ((pinned (calendar-absolute-from-gregorian '(8 20 2026))))
  (advice-add 'org-today :override (lambda () pinned))
  ...
  (advice-remove 'org-today ...))
```

否则夹具会随时间推移而静默失效——这是这类夹具最常见、也最难察觉的腐坏方式。

## 必需的设置

```elisp
(require 'org-habit)                    ; 否则习惯条目退化为普通 scheduled 条目
(setq org-enforce-todo-dependencies t          ; ORDERED blocking
      org-enforce-todo-checkbox-dependencies t ; checkbox blocking — separate switch
      org-agenda-dim-blocked-tasks t           ; without it org-todo-blocked is never written
      org-habit-show-habits t)
```

`org-todo-blocked` 只在 `org-agenda-dim-blocked-tasks` 与 `org-blocker-hook`
同时非空时才被写入（`org-agenda.el:7413-7414`）。少设一项，阻塞相关的测试会
静默通过而实际未覆盖。

**两种阻塞是两个开关**，实测确认：只开 `org-enforce-todo-dependencies` 时
仅 ORDERED 生效，得到 2 个被阻塞条目；再开
`org-enforce-todo-checkbox-dependencies` 才把复选框那条算进来，变成 3 个。
断言应针对**去重后的条目数**，不要针对行数——同一条目会在每个 agenda 块里
各出现一次，行数随 `org-other-agenda-series` 的块数变化。

## 覆盖的语义

| 语义 | 夹具中的样本 | 实测结果 |
| --- | --- | --- |
| 阻塞（ORDERED） | 项目 B 的第二、三步 | `org-todo-blocked` 命中 |
| 阻塞（复选框） | 项目 C 的父任务 | 同上，合计 4 行 |
| 习惯 | 每日复盘、每周清空收件箱 | `org-habit-p` 命中 2 行 |
| 融合视图逐行类型 | 全部 | `agenda` 20 行 / `todo` 21 行 |
| 无日期待办 | 「无日期条目」小节 | 只出现在 alltodo 块 |
| 优先级档位 | `[#A]` / `[#B]` / `[#C]` / 无 cookie | 2000 / 1000 / 0 / 1000 |
| search 优先级写死 | 含 `zorblatt` 的两条 | search 视图中一律 1000 |
| 带时间段的条目 | 「今天安排的低优先级任务」 | 参与时间栅格对齐 |

## 未覆盖，需要真实数据

- 真实数据规模下的性能（本夹具仅十余条目）。
- 用户实际的 `org-blocker-hook` 配置。`org-entry-blocked-p` 会执行任意 hook，
  第三方（如 `org-depend`）的阻塞规则只有在真实配置下才试得出来。
- column view 的 summary 与 CLOCKSUM 在深层树上的计算。

## 正在计时的条目

「正在计时的任务」条目留了一条未闭合的 `CLOCK:` 行，但**仅有文本不足以**让
`org-clock-hd-marker` 生效。测试需在 setup 中对该条目显式 `org-clock-in`，
或调用 `org-resolve-clocks`。验证目标是行上 `org-hd-marker` 与
`org-clock-hd-marker` 相等这一谓词（`org-agenda.el:4106-4107`）。
