/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.eclipse.spi;

import java.io.File;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.datatools.connectivity.services.PluginResourceLocator;
import org.osgi.framework.BundleContext;

/**
 *
 */
public class KEclipseSPIPlugin extends Plugin {

    /**
     * This plugin's identifier.
     */
    public static final String PLUGIN_ID = KEclipseSPIPlugin.class.getPackage().getName();
    
    /**
     * Target directory
     */
    private static final String TARGET = "target"; //$NON-NLS-1$

    /**
     * sources jar component
     */
    private static final String SOURCES = "sources"; //$NON-NLS-1$

    /**
     * JAR File Extension
     */
    private static final String JAR = "jar"; //$NON-NLS-1$

    /**
     * The package identifier.
     */
    public static final String PACKAGE_ID = KEclipseSPIPlugin.class.getPackage().getName();

    /**
     * The shared instance.
     */
    private static KEclipseSPIPlugin plugin;

    @Override
    public void start( final BundleContext context ) throws Exception {
        super.start(context);
        plugin = this;
    }

    /**
     * Logs the given message and {@link Throwable}with the supplied severity.
     *
     * @param severity the severity, which corresponds to the {@link IStatus#getSeverity() IStatus severity}.
     * @param message the message to be logged
     * @param t the exception; may be null
     */
    private static void log(int error, Throwable throwable, String message) {
        plugin.getLog().log(new Status(IStatus.ERROR, PLUGIN_ID, message, throwable));
    }

    /**
     * Logs the given Throwable.
     * <p>
     * If this class is initialized by the Eclipse Platform, then this will forward the request to the
     * {@link org.eclipse.core.runtime.ILog#log(org.eclipse.core.runtime.IStatus)}method.
     * </p>
     *
     * @param throwable the Throwable to log; may not be null
     */
    public static void log(Throwable throwable) {
        log(IStatus.ERROR, throwable, throwable.getLocalizedMessage());
    }
    
    /**
     * Find the plugin jar based on the given path
     *
     * @param path
     * @return
     */
    private static String findPluginJar(IPath path) {
        if (path == null)
            return null;

        if (!path.getFileExtension().equals(JAR))
            path = path.addFileExtension(JAR);

        String osPath = path.toOSString();
        File jarFile = new File(osPath);
        if (jarFile.exists())
            return osPath;

        return null;
    }

    /**
     * Find a built jar in a 'target' sub-directory of the given path location.
     *
     * This location only really occurs when developing / testing and a built
     * version is available in the maven target directory.
     *
     * @param path
     * @return
     */
    private static String findTargetJar(IPath path) {
        if (path == null)
            return null;

        path = path.append(TARGET);

        File targetDir = new File(path.toOSString());
        if (! targetDir.isDirectory())
            return null;

        for (File file : targetDir.listFiles()) {
            if (! file.getName().endsWith(JAR))
                continue;

            // Ignore sources jar
            if (file.getName().contains(SOURCES))
                continue;

            if (file.getName().startsWith(PLUGIN_ID))
                return file.getAbsolutePath();
        }

        return null;
    }

    /**
     * The location of this plugin from the filesystem
     *
     * @param pluginId id of the plugin to find
     *
     * @return path of plugin
     */
    public static String getPluginPath(String pluginId) {
        IPath path = PluginResourceLocator.getPluginRootPath(pluginId);
        String jarPath = findPluginJar(path);

        if (jarPath == null) {
            /*
             * Should normally exist in installed environment but when developing it will not.
             * Use the backup version of checking the target directory for a build jar of this plugin.
             */
            jarPath = findTargetJar(path);
        }

        return jarPath;
    }
}
