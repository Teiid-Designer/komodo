/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.modeshape.test.utils;

import static org.junit.Assert.fail;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.observation.Event;

/**
 * Listener that checks newly sequenced nodes to see if they equals given paths
 */
public class SequencerListener implements EventListener {

    private final List<String> pathsToBeSequenced = new ArrayList<String>();

    private final CountDownLatch updateLatch;

    /**
     * @param pathsToBeSequenced paths to look for during sequencing
     * @param updateLatch countdown latch to change when paths are found
     */
    public SequencerListener(List<String> pathsToBeSequenced, CountDownLatch updateLatch) {
        this.pathsToBeSequenced.addAll(pathsToBeSequenced);
        this.updateLatch = updateLatch;
    }

    @Override
    public void onEvent(EventIterator events) {
        while (events.hasNext()) {
            try {
                Event event = (Event)events.nextEvent();

                String nodePath = event.getPath();

                Iterator<String> pathSeqIter = pathsToBeSequenced.iterator();
                while (pathSeqIter.hasNext()) {
                    String pathToBeSequenced = pathSeqIter.next();

                    if (nodePath.matches(pathToBeSequenced))
                        pathSeqIter.remove();
                }

                if (pathsToBeSequenced.isEmpty()) {
                    KLog.getLogger().info("Testing latch pattern against node path: " + nodePath + ": Passed"); //$NON-NLS-1$ //$NON-NLS-2$
                    updateLatch.countDown();
                } else {
                    KLog.getLogger().info("Testing latch pattern against node path: " + nodePath + ": Failed"); //$NON-NLS-1$ //$NON-NLS-2$
                }

            } catch (Exception ex) {
                fail(ex.getMessage());
            }
        }
    }
}